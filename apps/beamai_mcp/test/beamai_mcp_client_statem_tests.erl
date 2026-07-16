%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_client 状态机测试
%%%
%%% 这个 572 行的 gen_statem 此前零测试，藏着三个"根本不能用"级缺陷：
%%%
%%% 1. **永远连不上**：{response,...} 的唯一产生点在 recv_loop 里，而 recv_loop
%%%    只在进入 connected 时武装；进入 connected 又需要先收到 initialize 响应
%%%    ——鸡生蛋死锁。任何传输、任何行为端正的 server，客户端都在 connecting
%%%    干等到 init_timeout 然后掉 disconnected。
%%% 2. **transport 失败崩客户端**：connecting(enter) 里返回 {next_state,...}，
%%%    enter 回调不允许改状态 → bad_state_enter_return_from_state_function，
%%%    干净的 {error, econnrefused} 变成进程崩溃并带崩 start_link 调用方。
%%% 3. **disconnected 收到 state_timeout / 迟到 info 即 function_clause**。
%%%
%%% 全部经 meck 注入 beamai_mcp_mock_transport 验证。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_client_statem_tests).

-include_lib("eunit/include/eunit.hrl").

client_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun connects_against_wellbehaved_server/0,
      fun list_tools_works_after_connect/0,
      fun call_tool_works_after_connect/0,
      fun transport_failure_does_not_crash/0,
      fun mute_server_times_out_to_disconnected/0,
      fun late_recv_loop_does_not_crash_disconnected/0,
      fun initialize_on_connected_replies_instead_of_crashing/0,
      fun stray_info_in_connected_is_ignored/0]}.

setup() ->
    catch meck:unload(beamai_mcp_transport),
    meck:new(beamai_mcp_transport, [passthrough]),
    mock_transport(#{}),
    ok.

cleanup(_) ->
    catch meck:unload(beamai_mcp_transport),
    ok.

%%====================================================================
%% 用例
%%====================================================================

%% 回归测试（缺陷 1）：以前任何配置下都到不了 connected
connects_against_wellbehaved_server() ->
    Pid = start(),
    ?assertEqual(connected, wait_for_state(Pid, connected, 2000)),
    stop(Pid).

list_tools_works_after_connect() ->
    Pid = start(),
    connected = wait_for_state(Pid, connected, 2000),
    ?assertMatch({ok, #{<<"tools">> := [#{<<"name">> := <<"mock_tool">>}]}},
                 beamai_mcp_client:list_tools(Pid)),
    stop(Pid).

call_tool_works_after_connect() ->
    Pid = start(),
    connected = wait_for_state(Pid, connected, 2000),
    ?assertMatch({ok, #{<<"content">> := [_]}},
                 beamai_mcp_client:call_tool(Pid, <<"mock_tool">>, #{})),
    stop(Pid).

%% 回归测试（缺陷 2）：干净的 {error,_} 曾变成 bad_state_enter_return 崩溃
transport_failure_does_not_crash() ->
    meck:expect(beamai_mcp_transport, create, fun(_) -> {error, econnrefused} end),
    process_flag(trap_exit, true),
    {ok, Pid} = beamai_mcp_client:start_link(#{transport => stdio}),
    timer:sleep(150),
    ?assert(is_process_alive(Pid)),
    ?assertEqual({ok, disconnected}, beamai_mcp_client:get_state(Pid)),
    stop(Pid).

%% server 永不应答：init_timeout 到点降级为 disconnected，进程存活
mute_server_times_out_to_disconnected() ->
    mock_transport(#{mute => true}),
    Pid = start(#{init_timeout => 300}),
    ?assertEqual(disconnected, wait_for_state(Pid, disconnected, 2000)),
    ?assert(is_process_alive(Pid)),
    stop(Pid).

%% 回归测试（缺陷 3）：掉到 disconnected 后，在途的 recv_loop 迟到消息
%% 曾触发 function_clause 崩溃
late_recv_loop_does_not_crash_disconnected() ->
    mock_transport(#{mute => true}),
    Pid = start(#{init_timeout => 200}),
    disconnected = wait_for_state(Pid, disconnected, 2000),
    %% 直接投递一个迟到的 recv_loop + 一个随机 info
    Pid ! recv_loop,
    Pid ! {port_exit, fake},
    timer:sleep(150),
    ?assert(is_process_alive(Pid)),
    ?assertEqual({ok, disconnected}, beamai_mcp_client:get_state(Pid)),
    stop(Pid).

%% 对已连接客户端调 initialize：要回复错误，不能 function_clause
initialize_on_connected_replies_instead_of_crashing() ->
    Pid = start(),
    connected = wait_for_state(Pid, connected, 2000),
    ?assertMatch({error, {unsupported_in_state, connected, _}},
                 beamai_mcp_client:initialize(Pid)),
    ?assert(is_process_alive(Pid)),
    stop(Pid).

%% connected 收到端口退出等杂项 info：吞掉，不崩
stray_info_in_connected_is_ignored() ->
    Pid = start(),
    connected = wait_for_state(Pid, connected, 2000),
    Pid ! {some_port, {exit_status, 1}},
    Pid ! {'EXIT_lookalike', x},
    timer:sleep(150),
    ?assert(is_process_alive(Pid)),
    ?assertEqual({ok, connected}, beamai_mcp_client:get_state(Pid)),
    stop(Pid).

%%====================================================================
%% 辅助
%%====================================================================

mock_transport(MockCfg) ->
    meck:expect(beamai_mcp_transport, create, fun(_ClientCfg) ->
        {ok, St} = beamai_mcp_mock_transport:connect(MockCfg),
        {ok, {beamai_mcp_mock_transport, St}}
    end).

start() -> start(#{}).

start(Extra) ->
    {ok, Pid} = beamai_mcp_client:start_link(maps:merge(#{transport => stdio}, Extra)),
    Pid.

stop(Pid) -> catch beamai_mcp_client:stop(Pid), ok.

%% 轮询等待状态；到点返回实际状态便于断言输出可读
wait_for_state(Pid, Want, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_loop(Pid, Want, Deadline).

wait_loop(Pid, Want, Deadline) ->
    case beamai_mcp_client:get_state(Pid) of
        {ok, Want} -> Want;
        {ok, Other} ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> Other;
                false -> timer:sleep(50), wait_loop(Pid, Want, Deadline)
            end
    end.

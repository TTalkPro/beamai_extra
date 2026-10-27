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
      fun transport_failure_reason_is_exposed/0,
      fun await_connected_returns_ok/0,
      fun recv_failure_replies_pending_call/0,
      fun wait_result_keeps_transport_state/0,
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

%% 回归：连接失败不能只转 disconnected；await_connected 和后续调用
%% 都必须拿到 econnrefused。
transport_failure_reason_is_exposed() ->
    meck:expect(beamai_mcp_transport, create, fun(_) -> {error, econnrefused} end),
    {ok, Pid} = beamai_mcp_client:start_link(#{transport => stdio}),
    ?assertEqual({error, {connection_failed, econnrefused}},
                 beamai_mcp_client:await_connected(Pid, 1000)),
    ?assertEqual({error, {not_connected, econnrefused}},
                 beamai_mcp_client:list_tools(Pid)),
    stop(Pid).

%% await_connected 的成功路径：start_link 保持异步，但调用方可显式等握手。
await_connected_returns_ok() ->
    Pid = start(),
    ?assertEqual(ok, beamai_mcp_client:await_connected(Pid, 2000)),
    stop(Pid).

%% 回归：connected 下传输层读取失败时，pending call 不能干等到超时。
recv_failure_replies_pending_call() ->
    mock_transport(#{fail_on_call => 1}),
    Pid = start(),
    connected = wait_for_state(Pid, connected, 2000),
    Result = beamai_mcp_client:call_tool(Pid, <<"mock_tool">>, #{}, 3000),
    ?assertEqual({error, {connection_failed, transport_closed}}, Result),
    ?assertEqual({ok, disconnected}, beamai_mcp_client:get_state(Pid)),
    stop(Pid).

%% server 永不应答：init_timeout 到点降级为 disconnected，进程存活
mute_server_times_out_to_disconnected() ->
    mock_transport(#{mute => true}),
    Pid = start(#{init_timeout => 300}),
    ?assertEqual(disconnected, wait_for_state(Pid, disconnected, 2000)),
    ?assert(is_process_alive(Pid)),
    stop(Pid).

%% 回归（2026-10-27）：传输层 {wait, NewState} 表示“本轮没拼出完整消息，
%% 但 state 已经推进”。客户端要么保存它继续轮询，要么至少别把它当成
%% 陌生返回值崩掉——这里用 wait_rounds 计数把它锁死。
wait_result_keeps_transport_state() ->
    mock_transport(#{wait_rounds => 5}),
    Pid = start(),
    ?assertEqual(ok, beamai_mcp_client:await_connected(Pid, 2000)),
    ?assertMatch({ok, #{<<"tools">> := [_]}}, beamai_mcp_client:list_tools(Pid)),
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

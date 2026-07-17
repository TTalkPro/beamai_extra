%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_server 集成测试（gen_server 层）
%%%
%%% 补齐直接覆盖：此前只有 beamai_a2a_server_encoding_tests（编码）和
%%% http_handler/input_required 的间接触及。这里测 gen_server 公开面里
%%% **不依赖 LLM/agent** 的路径：agent card、以及 handle_json 经
%%% JSON 解码 → do_handle_request → beamai_a2a_handler:dispatch 的完整链路。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_server_tests).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun get_agent_card_returns_config/1,
      fun handle_json_unknown_method/1,
      fun handle_json_tasks_get_unknown/1,
      fun handle_request_map_unknown_method/1,
      fun handle_json_parse_error/1,
      fun batch_returns_json_array/1,
      fun batch_preserves_order_and_ids/1,
      fun batch_single_element/1]}.

setup() ->
    {ok, Pid} = beamai_a2a_server:start_link(#{
        agent_config => #{
            name => <<"srv-test-agent">>,
            description => <<"server integration test">>,
            url => <<"http://localhost/a2a">>,
            version => <<"2.0.0">>
        }
    }),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    ok.

%% agent card 反映 start_link 时的 agent_config
get_agent_card_returns_config(Pid) ->
    ?_test(begin
        {ok, Card} = beamai_a2a_server:get_agent_card(Pid),
        ?assert(is_map(Card)),
        %% 内部 card 用原子键（序列化时才转 binary）
        ?assertEqual(<<"srv-test-agent">>, maps:get(name, Card))
    end).

%% handle_json：合法 JSON + 未知方法 → 经 dispatch 得 -32601
handle_json_unknown_method(Pid) ->
    ?_test(begin
        Req = beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 7,
                                         <<"method">> => <<"no/such">>, <<"params">> => #{}}),
        {ok, Bin} = beamai_a2a_server:handle_json(Pid, Req),
        Resp = json:decode(Bin),
        ?assertEqual(-32601, err_code(Resp)),
        ?assertEqual(7, maps:get(<<"id">>, Resp))
    end).

%% handle_json：tasks/get 查未知任务 → -32001
handle_json_tasks_get_unknown(Pid) ->
    ?_test(begin
        Req = beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 8,
                                         <<"method">> => <<"tasks/get">>,
                                         <<"params">> => #{<<"taskId">> => <<"ghost">>}}),
        {ok, Bin} = beamai_a2a_server:handle_json(Pid, Req),
        ?assertEqual(-32001, err_code(json:decode(Bin)))
    end).

%% handle_request（已解码 map 形式）→ 同样经 dispatch
handle_request_map_unknown_method(Pid) ->
    ?_test(begin
        Request = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 9,
                    <<"method">> => <<"bogus">>, <<"params">> => #{}},
        {ok, Resp} = beamai_a2a_server:handle_request(Pid, Request),
        ?assertEqual(-32601, err_code(Resp))
    end).

%% 畸形 JSON → parse_error（单次编码，decode 得对象；与 encoding_tests 呼应）
handle_json_parse_error(Pid) ->
    ?_test(begin
        {ok, Bin} = beamai_a2a_server:handle_json(Pid, <<"{bad json">>),
        Resp = json:decode(Bin),
        ?assert(is_map(Resp)),
        ?assertEqual(-32700, err_code(Resp))
    end).

%%====================================================================
%% Batch（JSON-RPC 2.0 批处理）
%%
%% 关键：batch 响应必须是一个 JSON **数组**（每元素是独立响应对象），
%% 且只编码一次。这依赖 dispatch/handler 路径返回 map、边界统一 encode——
%% 若某响应被提前编码成 binary，塞进数组再 encode 就会双重编码（数组元素
%% 变成字符串）。此前无测试守着，仅靠 probe 验过。
%%====================================================================

%% batch 请求 → decode 后是含 N 个对象的数组
batch_returns_json_array(Pid) ->
    ?_test(begin
        Batch = beamai_utils:encode_json([
            rpc(1, <<"tasks/get">>, #{<<"taskId">> => <<"a">>}),
            rpc(2, <<"tasks/get">>, #{<<"taskId">> => <<"b">>})
        ]),
        {ok, Bin} = beamai_a2a_server:handle_json(Pid, Batch),
        Decoded = json:decode(Bin),
        ?assert(is_list(Decoded)),                 %% ← 双重编码时这里会是 binary
        ?assertEqual(2, length(Decoded)),
        %% 每个元素本身是响应对象（map），不是被再编码的字符串
        [?assert(is_map(E)) || E <- Decoded]
    end).

%% batch 保持顺序与 id 对应
batch_preserves_order_and_ids(Pid) ->
    ?_test(begin
        Batch = beamai_utils:encode_json([
            rpc(11, <<"tasks/get">>, #{<<"taskId">> => <<"x">>}),
            rpc(22, <<"bogus/method">>, #{}),
            rpc(33, <<"tasks/get">>, #{<<"taskId">> => <<"z">>})
        ]),
        {ok, Bin} = beamai_a2a_server:handle_json(Pid, Batch),
        Decoded = json:decode(Bin),
        ?assertEqual([11, 22, 33], [maps:get(<<"id">>, E) || E <- Decoded]),
        %% 第 2 个是未知方法 → -32601，其余是任务未找到 → -32001
        ?assertEqual([-32001, -32601, -32001],
                     [err_code(E) || E <- Decoded])
    end).

%% 单元素 batch 仍返回数组（不退化成单对象）
batch_single_element(Pid) ->
    ?_test(begin
        Batch = beamai_utils:encode_json([rpc(1, <<"bogus">>, #{})]),
        {ok, Bin} = beamai_a2a_server:handle_json(Pid, Batch),
        Decoded = json:decode(Bin),
        ?assert(is_list(Decoded)),
        ?assertEqual(1, length(Decoded))
    end).

%%====================================================================
%% 辅助
%%====================================================================

rpc(Id, Method, Params) ->
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
      <<"method">> => Method, <<"params">> => Params}.

err_code(Resp) ->
    maps:get(<<"code">>, maps:get(<<"error">>, Resp)).

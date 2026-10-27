%%%-------------------------------------------------------------------
%%% @doc 测试桩：行为端正的假 MCP server 传输层。
%%%
%%% send/2 收到请求立刻把符合规范的响应排进队列，recv/2 从队列取。
%%% 状态经客户端 #data 全程穿线——响应就躺在客户端手里，看它读不读。
%%% 可配置 mute（收请求不给响应），模拟永不应答的 server。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_mock_transport).

-export([connect/1, send/2, recv/2, close/1, is_connected/1]).

connect(Config) ->
    {ok, #{queue => [], mute => maps:get(mute, Config, false),
           fail_next_recv => false,
           %% 前 N 次 recv 只交回半成品（{wait, NewState}），之后才有完整消息。
           wait_rounds => maps:get(wait_rounds, Config, 0),
           fail_on_call => maps:get(fail_on_call, Config, 0),
           call_count => 0}}.

send(_Message, #{mute := true} = State) ->
    {ok, State};
send(Message, #{queue := Q} = State) ->
    case json:decode(Message) of
        #{<<"method">> := <<"initialize">>, <<"id">> := Id} ->
            Resp = beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
                <<"result">> => #{
                    <<"protocolVersion">> => <<"2024-11-05">>,
                    <<"capabilities">> => #{<<"tools">> => #{}},
                    <<"serverInfo">> => #{<<"name">> => <<"mock">>,
                                          <<"version">> => <<"1">>}}}),
            {ok, State#{queue => Q ++ [Resp]}};
        #{<<"method">> := <<"tools/list">>, <<"id">> := Id} ->
            Resp = beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
                <<"result">> => #{<<"tools">> => [#{<<"name">> => <<"mock_tool">>,
                    <<"description">> => <<"a tool">>,
                    <<"inputSchema">> => #{<<"type">> => <<"object">>}}]}}),
            {ok, State#{queue => Q ++ [Resp]}};
        #{<<"method">> := <<"tools/call">>, <<"id">> := Id} ->
            Resp = beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
                <<"result">> => #{<<"content">> =>
                    [#{<<"type">> => <<"text">>, <<"text">> => <<"tool ran">>}],
                    <<"isError">> => false}}),
            %% 模拟响应读取阶段失败，验证 client 会立刻回复在途调用。
            Count = maps:get(call_count, State, 0) + 1,
            FailOn = maps:get(fail_on_call, State, 0),
            %% 目标失败时响应根本不从 transport 露出。
            Queued = case FailOn =:= Count of
                true -> Q;
                false -> Q ++ [Resp]
            end,
            {ok, State#{queue => Queued,
                        call_count => Count,
                        fail_next_recv => FailOn =:= Count}};
        _ ->
            %% initialized 通知等：无响应
            {ok, State}
    end.

%% 传输层契约：{wait, NewState} = 本轮没有完整消息，但 NewState 里可能已经
%% 攒下了分块，调用方必须保存它再轮询。丢 state 的客户端在这里会永远读不到
%% wait_rounds 递减后的那条响应。
recv(_Timeout, #{wait_rounds := N} = State) when N > 0 ->
    {wait, State#{wait_rounds => N - 1}};
recv(_Timeout, #{queue := [H | T]} = State) -> {ok, H, State#{queue => T}};
recv(_Timeout, #{fail_next_recv := true} = _State) ->
    {error, transport_closed};
recv(_Timeout, #{queue := []} = _State) -> {error, timeout}.

close(_) -> ok.
is_connected(_) -> true.

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
    {ok, #{queue => [], mute => maps:get(mute, Config, false)}}.

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
            {ok, State#{queue => Q ++ [Resp]}};
        _ ->
            %% initialized 通知等：无响应
            {ok, State}
    end.

recv(_Timeout, #{queue := [H | T]} = State) -> {ok, H, State#{queue => T}};
recv(_Timeout, #{queue := []} = State) -> {error, timeout}.

close(_) -> ok.
is_connected(_) -> true.

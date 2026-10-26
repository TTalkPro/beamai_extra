%%% @doc 原始 HTTP handler，精确控制状态码、body 和协议头断言。
-module(beamai_mcp_http_probe_handler).

-export([init/2]).

init(Req0, Mode) ->
    Req = case Mode of
        raw_unauthorized ->
            cowboy_req:reply(401,
                             #{<<"content-type">> => <<"text/plain">>},
                             <<"unauthorized-body">>, Req0);
        raw_large ->
            cowboy_req:reply(200,
                             #{<<"content-type">> => <<"application/json">>},
                             binary:copy(<<"x">>, 2048), Req0);
        raw_protocol ->
            handle_protocol(Req0)
    end,
    {ok, Req, Mode}.

handle_protocol(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Decoded = json:decode(Body),
    case Decoded of
        #{<<"method">> := <<"notifications/initialized">>} ->
            cowboy_req:reply(202, #{}, <<>>, Req1);
        _ ->
            handle_rpc(Decoded, Req1)
    end.

handle_rpc(#{<<"id">> := Id, <<"method">> := Method}, Req1) ->
    case {Method, cowboy_req:header(<<"mcp-session-id">>, Req1)} of
        {<<"initialize">>, undefined} ->
            Result = #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{<<"tools">> => #{}},
                <<"serverInfo">> => #{<<"name">> => <<"probe">>,
                                      <<"version">> => <<"1">>}
            },
            reply_rpc(Id, Result, #{<<"mcp-session-id">> => <<"probe-session">>}, Req1);
        {<<"tools/list">>, <<"probe-session">>} ->
            case protocol_header(Req1) of
                correct ->
                    reply_rpc(Id, #{<<"tools">> => [#{<<"name">> => <<"probe">>}]},
                              #{}, Req1);
                wrong ->
                    cowboy_req:reply(400, #{}, <<"bad protocol header">>, Req1)
            end
    end.

protocol_header(Req) ->
    case cowboy_req:header(<<"mcp-protocol-version">>, Req) of
        <<"2025-11-25">> -> correct;
        _ -> wrong
    end.

reply_rpc(Id, Result, Headers, Req) ->
    Body = json:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
                         <<"result">> => Result}),
    cowboy_req:reply(200,
                     Headers#{<<"content-type">> => <<"application/json">>},
                     Body, Req).

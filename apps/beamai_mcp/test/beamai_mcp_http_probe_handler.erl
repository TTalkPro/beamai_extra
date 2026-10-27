%%% @doc 原始 HTTP handler，精确控制状态码、body 和协议头断言。
-module(beamai_mcp_http_probe_handler).

-export([init/2]).

-define(CHUNK_BYTES, 4096).
-define(CHUNK_GAP_MS, 100).

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
        %% 响应体分多次 write 送达：复现 2026-10-27 反馈——recv(0) 轮询超时
        %% 会丢掉本轮已收到的 body 分块，大响应永远拼不完整。
        raw_chunked_json ->
            handle_chunked(Req0, json);
        raw_chunked_sse ->
            handle_chunked(Req0, sse);
        raw_protocol ->
            handle_protocol(Req0)
    end,
    {ok, Req, Mode}.

%% @private 大响应拆成多帧发出；initialize 仍走普通回复，先让客户端连上。
handle_chunked(Req0, Kind) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Decoded = json:decode(Body),
    Id = maps:get(<<"id">>, Decoded, undefined),
    case maps:get(<<"method">>, Decoded, undefined) of
        <<"notifications/initialized">> ->
            cowboy_req:reply(202, #{}, <<>>, Req1);
        <<"initialize">> ->
            reply_rpc(Id, #{<<"protocolVersion">> => <<"2025-11-25">>,
                            <<"capabilities">> => #{<<"tools">> => #{}},
                            <<"serverInfo">> => #{<<"name">> => <<"probe">>,
                                                  <<"version">> => <<"1">>}},
                      #{}, Req1);
        _Method ->
            Payload = rpc_json(Id, chunked_text(Kind)),
            case Kind of
                json ->
                    Req2 = cowboy_req:stream_reply(
                             200, #{<<"content-type">> => <<"application/json">>}, Req1),
                    %% 每片一次 write，片间留足间隔，确保它们落在不同的轮询周期里
                    stream_json_pieces(Payload, Req2);
                sse ->
                    %% SSE 帧被切断：data 的前半和结束空行分属两次 write
                    {Head, Tail} = split_at(Payload, byte_size(Payload) div 2),
                    %% cowboy 的 stream_body/3 返回 ok，Req 本身继续可用
                    Req2 = cowboy_req:stream_reply(
                             200, #{<<"content-type">> => <<"text/event-stream">>}, Req1),
                    ok = cowboy_req:stream_body(<<"event: message\ndata: ", Head/binary>>,
                                                nofin, Req2),
                    timer:sleep(?CHUNK_GAP_MS * 3),
                    ok = cowboy_req:stream_body(<<Tail/binary, "\n\n">>, nofin, Req2),
                    ok = cowboy_req:stream_body(<<>>, fin, Req2),
                    Req2
            end
    end.

chunked_text(json) ->
    #{<<"content">> => [#{<<"type">> => <<"text">>,
                          <<"text">> => binary:copy(<<"chunked-body-payload-"/utf8>>, 1200)}],
      <<"isError">> => false};
chunked_text(sse) ->
    #{<<"content">> => [#{<<"type">> => <<"text">>,
                          <<"text">> => binary:copy(<<"sse-half-event-"/utf8>>, 600)}],
      <<"isError">> => false}.

rpc_json(Id, Result) ->
    iolist_to_binary(json:encode(#{<<"jsonrpc">> => <<"2.0">>,
                                   <<"id">> => Id,
                                   <<"result">> => Result})).

stream_json_pieces(<<>>, Req) ->
    ok = cowboy_req:stream_body(<<>>, fin, Req),
    Req;
stream_json_pieces(Payload, Req) ->
    {Head, Tail} = split_at(Payload, ?CHUNK_BYTES),
    ok = cowboy_req:stream_body(Head, nofin, Req),
    timer:sleep(?CHUNK_GAP_MS),
    stream_json_pieces(Tail, Req).

split_at(Bin, Size) when byte_size(Bin) =< Size -> {Bin, <<>>};
split_at(Bin, Size) ->
    {binary:part(Bin, 0, Size), binary:part(Bin, Size, byte_size(Bin) - Size)}.

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
    cowboy_req:reply(200,
                     Headers#{<<"content-type">> => <<"application/json">>},
                     rpc_json(Id, Result), Req).

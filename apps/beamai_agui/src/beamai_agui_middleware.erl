%%%-------------------------------------------------------------------
%%% @doc AG-UI HTTP authentication and rate-limit gate.
%%%
%%% Authentication runs first; a successfully authenticated key is also the
%%% rate-limit identity. When authentication is disabled, proxy forwarding
%%% headers identify the client for rate limiting. Missing services mean both
%%% gates are off, preserving the optional/runtime-free use of the handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_middleware).

-export([check/1]).

-type headers() :: [{binary(), binary()}].
-type result() :: {ok, map()} |
                  {error, pos_integer(), binary(), binary(),
                   [{binary(), binary()}]}.

-export_type([headers/0, result/0]).

%% @doc Run authentication and then rate limiting for one request.
%%
%% The error tuple contains HTTP status, machine-readable code, safe detail and
%% response headers such as `retry-after'. CORS is added by the HTTP adapter.
-spec check(headers()) -> result().
check(Headers) when is_list(Headers) ->
    case auth_enabled() of
        true ->
            case beamai_agui_auth:authenticate(Headers) of
                {ok, Auth} ->
                    rate_limit(Headers, rate_key(Auth));
                {error, auth_disabled} ->
                    rate_limit(Headers, <<"anonymous">>);
                {error, missing_api_key} ->
                    auth_error(401, <<"auth_required">>,
                               <<"Provide Authorization: Bearer <key> or X-API-Key">>);
                {error, invalid_api_key} ->
                    auth_error(401, <<"auth_failed">>, <<"Invalid API key">>);
                {error, key_expired} ->
                    auth_error(401, <<"auth_expired">>, <<"API key expired">>)
            end;
        false ->
            rate_limit(Headers, rate_key_from_headers(Headers))
    end.

rate_limit(_Headers, Key) ->
    case rate_enabled() of
        false ->
            {ok, #{authentication => auth_enabled(),
                   rate_limit => false}};
        true ->
            case beamai_agui_rate_limit:check_and_consume(Key) of
                {ok, Info} ->
                    {ok, #{authentication => auth_enabled(),
                           rate_limit => true, info => Info}};
                {error, rate_limited, Info} ->
                    RetryAfter = max(1, ceil(maps:get(retry_after, Info, 1000) / 1000)),
                    Detail = iolist_to_binary(io_lib:format(
                                "rate limit exceeded (~ts window)", [window(Info)])),
                    {error, 429, <<"rate_limited">>, Detail,
                     [{<<"retry-after">>, integer_to_binary(RetryAfter)}]};
                {error, rate_limit_disabled, _} ->
                    {ok, #{authentication => auth_enabled(),
                           rate_limit => false}}
            end
    end.

auth_error(Status, Code, Detail) ->
    {error, Status, Code, Detail, []}.

auth_enabled() ->
    try
        beamai_agui_auth:is_enabled()
    catch
        _:_ -> false
    end.

rate_enabled() ->
    try
        beamai_agui_rate_limit:is_enabled()
    catch
        _:_ -> false
    end.

rate_key(#{key := Key}) -> Key.

rate_key_from_headers(Headers) ->
    case header(Headers, <<"x-forwarded-for">>) of
        undefined ->
            case header(Headers, <<"x-real-ip">>) of
                undefined -> <<"anonymous">>;
                IP -> <<"ip:", IP/binary>>
            end;
        ForwardedFor ->
            [First | _] = binary:split(ForwardedFor, <<",">>),
            <<"ip:", (string:trim(First))/binary>>
    end.

header([], _Name) -> undefined;
header([{Name, Value} | _], Name) when is_binary(Name), is_binary(Value) -> Value;
header([{Name0, Value} | Rest], Name) when is_binary(Name0), is_binary(Value) ->
    case string:lowercase(Name0) =:= Name of
        true -> Value;
        false -> header(Rest, Name)
    end;
header([_ | Rest], Name) -> header(Rest, Name).

window(#{window := Value}) -> Value;
window(_) -> <<"request">>.

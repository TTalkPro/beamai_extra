%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_server 响应编码回归测试
%%%
%%% 历史缺陷（双重编码）：beamai_a2a_jsonrpc 的错误构造器（parse_error 等）
%%% **已返回编码好的 binary**，而 handle_json / http_handler 的调用点又套了一层
%%% jsx:encode——结果是"装着 JSON 的 JSON 字符串"：客户端 jsx:decode 后拿到的是
%%% binary 字符串 `<<"{...}">>` 而非错误对象 `#{<<"error">> => ...}`，
%%% 即一个畸形（spec 违规）的 JSON-RPC 响应。
%%%
%%% 根因：a2a 有两套返回约定相反的错误构造器——middleware 的
%%% make_*_error_response 返回 map（需 encode），jsonrpc 的返回 binary（勿 encode），
%%% 调用点一律 jsx:encode 就对前者对、对后者错。
%%%
%%% 这里锁住：错误响应必须 decode 一次即得 JSON 对象（map）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_server_encoding_tests).

-include_lib("eunit/include/eunit.hrl").

encoding_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun parse_error_is_single_encoded/1,
      fun parse_error_with_auth_is_single_encoded/1]}.

setup() ->
    {ok, Pid} = beamai_a2a_server:start_link(#{
        agent_config => #{
            name => <<"test-agent">>,
            description => <<"encoding test">>,
            url => <<"http://localhost/a2a">>,
            version => <<"1.0.0">>
        }
    }),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    ok.

%% 畸形 JSON → parse_error 路径。返回 binary 必须 decode 一次即得对象。
parse_error_is_single_encoded(Pid) ->
    ?_test(begin
        {ok, Bin} = beamai_a2a_server:handle_json(Pid, <<"{not valid json">>),
        assert_jsonrpc_error_object(Bin, -32700)
    end).

%% 带认证入口的同一路径（server.erl:242）
parse_error_with_auth_is_single_encoded(Pid) ->
    ?_test(begin
        case beamai_a2a_server:handle_json_with_auth(Pid, <<"{not valid json">>, []) of
            {error, parse_error, Bin} -> assert_jsonrpc_error_object(Bin, -32700);
            {ok, Bin, _RL} -> assert_jsonrpc_error_object(Bin, -32700)
        end
    end).

%%====================================================================
%% 辅助
%%====================================================================

%% 关键断言：decode 一次得到的是 **map**（JSON 对象），不是 binary 字符串。
%% 双重编码时 jsx:decode 会返回 <<"{...}">>（binary），而非 #{...}。
assert_jsonrpc_error_object(Bin, ExpectedCode) ->
    ?assert(is_binary(Bin)),
    Decoded = jsx:decode(Bin, [return_maps]),
    ?assert(is_map(Decoded)),                       %% ← 双重编码时这里是 binary，会挂
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(is_map(Error)),
    ?assertEqual(ExpectedCode, maps:get(<<"code">>, Error)).

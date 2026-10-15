%%%-------------------------------------------------------------------
%%% @doc beamai_tool_security 的行为测试。
%%%
%%% 这个模块此前零覆盖，却是工具执行前的唯一一道闸：路径黑名单挡 `.git`、
%%% 命令黑名单挡 `rm -rf /`。闸门本身的判断逻辑（尤其是白名单前缀语义与
%%% 黑名单子串语义）必须有测试钉住，否则改坏了没人知道。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_security_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 路径黑名单
%%====================================================================

default_config_blocks_sensitive_paths_test() ->
    ?assertEqual({error, {path_blocked, <<"/repo/.git/config">>}},
                 beamai_tool_security:check_path(<<"/repo/.git/config">>)),
    ?assertEqual({error, {path_blocked, <<"/app/.env">>}},
                 beamai_tool_security:check_path(<<"/app/.env">>)),
    ?assertEqual({error, {path_blocked, <<"/app/node_modules/x/index.js">>}},
                 beamai_tool_security:check_path(<<"/app/node_modules/x/index.js">>)).

default_config_allows_ordinary_paths_test() ->
    ?assertEqual(ok, beamai_tool_security:check_path(<<"/repo/src/main.erl">>)),
    ?assertEqual(ok, beamai_tool_security:check_path(<<"/tmp/scratch.txt">>)).

%% 黑名单是子串匹配（不是路径分量匹配），所以 `.env` 也会挡掉
%% `foo.env.bak`。这是刻意取舍：宁可误伤也不要漏。
blocked_path_is_substring_match_test() ->
    ?assertMatch({error, {path_blocked, _}},
                 beamai_tool_security:check_path(<<"/app/config.env.bak">>)).

%%====================================================================
%% 路径白名单
%%====================================================================

empty_allowlist_permits_everything_not_blocked_test() ->
    Config = #{allowed_paths => [], blocked_paths => [<<".git">>]},
    ?assertEqual(ok, beamai_tool_security:check_path(<<"/any/where.txt">>, Config)).

allowlist_requires_prefix_match_test() ->
    Config = #{allowed_paths => [<<"/repo/src">>], blocked_paths => []},
    ?assertEqual(ok, beamai_tool_security:check_path(<<"/repo/src/a.erl">>, Config)),
    ?assertEqual({error, {path_not_allowed, <<"/repo/test/a.erl">>}},
                 beamai_tool_security:check_path(<<"/repo/test/a.erl">>, Config)).

%% 白名单只认**开头**匹配：路径中间出现允许目录不算允许。
allowlist_match_must_start_at_position_zero_test() ->
    Config = #{allowed_paths => [<<"/repo/src">>], blocked_paths => []},
    ?assertEqual({error, {path_not_allowed, <<"/tmp/repo/src/a.erl">>}},
                 beamai_tool_security:check_path(<<"/tmp/repo/src/a.erl">>, Config)).

%% 黑名单优先于白名单：同一个路径既在白名单前缀里、又含黑名单片段时拒绝。
blocked_wins_over_allowed_test() ->
    Config = #{allowed_paths => [<<"/repo">>], blocked_paths => [<<".git">>]},
    ?assertEqual({error, {path_blocked, <<"/repo/.git">>}},
                 beamai_tool_security:check_path(<<"/repo/.git">>, Config)).

missing_config_keys_fall_back_to_permissive_test() ->
    ?assertEqual(ok, beamai_tool_security:check_path(<<"/repo/a.erl">>, #{})),
    ?assertEqual(ok, beamai_tool_security:check_command(<<"ls -la">>, #{})).

%%====================================================================
%% 命令黑名单
%%====================================================================

default_config_blocks_dangerous_commands_test() ->
    ?assertEqual({error, {command_blocked, <<"rm -rf /">>}},
                 beamai_tool_security:check_command(<<"rm -rf /">>)),
    ?assertMatch({error, {command_blocked, _}},
                 beamai_tool_security:check_command(<<":(){ :|:& };:">>)),
    ?assertMatch({error, {command_blocked, _}},
                 beamai_tool_security:check_command(<<"dd if=/dev/zero of=/dev/sda">>)).

default_config_allows_ordinary_commands_test() ->
    ?assertEqual(ok, beamai_tool_security:check_command(<<"ls -la">>)),
    ?assertEqual(ok, beamai_tool_security:check_command(<<"rebar3 eunit">>)).

%% 命令黑名单也是子串匹配：危险命令出现在参数里同样被拒。
blocked_command_is_substring_match_test() ->
    ?assertMatch({error, {command_blocked, _}},
                 beamai_tool_security:check_command(<<"echo hi && rm -rf /">>)).

custom_blocked_commands_test() ->
    Config = #{blocked_commands => [<<"git push">>]},
    ?assertEqual({error, {command_blocked, <<"git push --force">>}},
                 beamai_tool_security:check_command(<<"git push --force">>, Config)),
    ?assertEqual(ok, beamai_tool_security:check_command(<<"rm -rf /">>, Config)).

%%====================================================================
%% 默认配置本身
%%====================================================================

default_config_shape_test() ->
    Config = beamai_tool_security:default_config(),
    ?assertEqual([], maps:get(allowed_paths, Config)),
    ?assert(lists:member(<<".git">>, maps:get(blocked_paths, Config))),
    ?assert(lists:member(<<"rm -rf /">>, maps:get(blocked_commands, Config))).

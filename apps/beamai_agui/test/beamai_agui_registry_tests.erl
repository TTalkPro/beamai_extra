%%%-------------------------------------------------------------------
%%% @doc Agent 注册表测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_registry_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Pid} = beamai_agui_registry:start_link(),
    Pid.

cleanup(Pid) ->
    stop_sync(Pid).

%% 同步停：ETS 表随属主进程消失，异步 exit 会让后续用例看到还没消失的表
stop_sync(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 1000 -> erlang:demonitor(MRef, [flush]), ok
    end.

registry_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        F = fun(_I, _Cfg) -> {ok, agent} end,
        [?_assertEqual(ok, beamai_agui_registry:register(<<"a">>, F)),
         ?_assertMatch({ok, _}, beamai_agui_registry:lookup(<<"a">>)),
         ?_assertEqual({error, not_found}, beamai_agui_registry:lookup(<<"nope">>)),
         %% 工厂必须是 Fun/2（Input + Callbacks），别的一律拒
         ?_assertEqual({error, badarg},
                       beamai_agui_registry:register(<<"b">>, fun(_) -> ok end)),
         ?_assertEqual({error, badarg},
                       beamai_agui_registry:register(not_binary, F))]
     end}.

%% 注册表没起来时 lookup/list 不该崩（比如宿主没启 beamai_agui 应用）
%%
%% 自带起停：不依赖上面那个 fixture 的执行顺序，也不依赖它的表已经消失
no_table_is_not_a_crash_test() ->
    {ok, Pid} = beamai_agui_registry:start_link(),
    ok = stop_sync(Pid),
    ?assertEqual({error, not_found}, beamai_agui_registry:lookup(<<"x">>)),
    ?assertEqual([], beamai_agui_registry:list()).

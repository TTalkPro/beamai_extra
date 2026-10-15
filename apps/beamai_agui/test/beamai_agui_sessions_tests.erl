%%%-------------------------------------------------------------------
%%% @doc Cross-node thread ownership: `global` elects one session per threadId.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_sessions_tests).

-include_lib("eunit/include/eunit.hrl").

-define(THREAD, <<"global-owner-thread">>).

ownership_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"distributed mode elects one owner", fun one_global_owner/0},
      {"forget drops both local and global ownership", fun forget_clears_owner/0},
      {"local mode never registers global ownership", fun local_mode_stays_local/0}]}.

setup() ->
    ok = application:set_env(beamai_agui, distributed_sessions, true),
    catch global:unregister_name(name(?THREAD)),
    {ok, SessionSup} = beamai_agui_session_sup:start_link(),
    {ok, Sessions} = beamai_agui_sessions:start_link(),
    #{session_sup => SessionSup, sessions => Sessions}.

cleanup(Context) ->
    _ = beamai_agui_sessions:forget(?THREAD),
    ok = stop_proc(Context, sessions),
    ok = stop_proc(Context, session_sup),
    catch global:unregister_name(name(?THREAD)),
    ok = application:unset_env(beamai_agui, distributed_sessions),
    ok.

stop_proc(Context, Key) ->
    Pid = maps:get(Key, Context),
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 2000 -> erlang:demonitor(MRef, [flush]) end.

one_global_owner() ->
    {ok, First} = beamai_agui_sessions:ensure(?THREAD),
    {ok, Second} = beamai_agui_sessions:ensure(?THREAD),
    ?assertEqual(First, Second),
    ?assertEqual({ok, First}, beamai_agui_sessions:whereis_session(?THREAD)),
    ?assertEqual(First, global:whereis_name(name(?THREAD))).

forget_clears_owner() ->
    {ok, _Pid} = beamai_agui_sessions:ensure(?THREAD),
    ok = beamai_agui_sessions:forget(?THREAD),
    ?assertEqual({error, not_found},
                 beamai_agui_sessions:whereis_session(?THREAD)),
    ?assertEqual(undefined, global:whereis_name(name(?THREAD))).

local_mode_stays_local() ->
    ok = application:set_env(beamai_agui, distributed_sessions, false),
    Thread = <<"local-only-thread">>,
    try
        {ok, Pid} = beamai_agui_sessions:ensure(Thread),
        ?assertEqual({ok, Pid}, beamai_agui_sessions:whereis_session(Thread)),
        ?assertEqual(undefined, global:whereis_name(name(Thread)))
    after
        beamai_agui_sessions:forget(Thread)
    end.

name(Thread) ->
    {beamai_agui_sessions, Thread}.

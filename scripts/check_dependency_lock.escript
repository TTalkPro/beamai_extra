#!/usr/bin/env escript
%%! -nologo

main([LockFile]) ->
    Expected = #{
        <<"beamai_core">> => "08aa844a11791fabc20ff2dab19ea82a71343881",
        <<"beamai_llm">> => "08aa844a11791fabc20ff2dab19ea82a71343881",
        <<"beamai_agent">> => "08aa844a11791fabc20ff2dab19ea82a71343881"
    },
    case file:consult(LockFile) of
        {ok, Terms} ->
            Locks = locks(Terms),
            refs(Locks, Expected),
            maps:foreach(
              fun(Name, Ref) ->
                  io:format("~ts ~ts~n", [Name, Ref])
              end,
              Expected),
            halt(0);
        {error, Reason} ->
            fail("cannot consult ~ts: ~p", [LockFile, Reason])
    end;
main(_) ->
    io:put_chars(standard_error, "usage: check_dependency_lock.escript rebar.lock\n"),
    halt(2).

locks([{_, Locks} | _]) when is_list(Locks) -> Locks;
locks([_ | Rest]) -> locks(Rest);
locks([]) -> fail("no lock terms found in rebar.lock", []).

refs(Locks, Expected) ->
    maps:foreach(
      fun(Name, ExpectedRef) ->
          case lists:keyfind(Name, 1, Locks) of
              false ->
                  fail("missing upstream dependency: ~ts", [Name]);
              {Name, {git_subdir, _Url, {ref, ExpectedRef}, _SubDir}, _Level} ->
                  ok;
              {Name, {git_subdir, _Url, {ref, ActualRef}, _SubDir}, _Level} ->
                  fail("~ts locked to ~ts, expected ~ts", [Name, ActualRef, ExpectedRef]);
              {Name, Actual, _Level} ->
                  fail("~ts is not a git_subdir ref lock: ~p", [Name, Actual])
          end
      end,
      Expected).

fail(Format, Args) ->
    io:put_chars(standard_error, io_lib:format("lock check failed: " ++ Format ++ "~n", Args)),
    halt(1).

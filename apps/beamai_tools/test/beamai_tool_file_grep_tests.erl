%%%-------------------------------------------------------------------
%%% @doc beamai_tool_file grep 行为测试
%%%
%%% 锁住 grep_files/3 的两个性质：
%%%   1. **正序**：匹配按文件顺序、文件内按行号递增返回。旧实现前向追加后又整体
%%%      lists:reverse，输出其实是**反序**的（潜在 bug，无测试锁定）；同时 O(n²)
%%%      （尾追加 + 每轮重算 length/1）。重写为 running counter + 反向累积一次 reverse。
%%%   2. **max_results 硬截断**：跨文件累计到上限即停。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_file_grep_tests).

-include_lib("eunit/include/eunit.hrl").

grep_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun forward_order_across_files/1,
      fun max_results_caps_across_files/1,
      fun no_match_returns_empty/1,
      fun single_file_line_order/1]}.

setup() ->
    Dir = test_dir(),
    file:make_dir(Dir),
    ok = file:write_file(filename:join(Dir, "a.txt"),
                         <<"alpha\nbeta match\ngamma\nmatch delta\n">>),
    ok = file:write_file(filename:join(Dir, "b.txt"),
                         <<"match one\nmatch two\nmatch three\n">>),
    Dir.

cleanup(Dir) ->
    [file:delete(F) || F <- filelib:wildcard(filename:join(Dir, "*"))],
    file:del_dir(Dir),
    ok.

%% 匹配必须按 文件序 + 行号递增 返回（回归：旧实现是反序）
forward_order_across_files(Dir) ->
    ?_test(begin
        Ms = grep(Dir, <<"match">>, 100),
        ?assertEqual(5, length(Ms)),
        Seq = [{basename(M), maps:get(line_number, M)} || M <- Ms],
        ?assertEqual([{<<"a.txt">>, 2}, {<<"a.txt">>, 4},
                      {<<"b.txt">>, 1}, {<<"b.txt">>, 2}, {<<"b.txt">>, 3}], Seq)
    end).

%% max_results 跨文件累计截断
max_results_caps_across_files(Dir) ->
    ?_test(begin
        Ms = grep(Dir, <<"match">>, 3),
        ?assertEqual(3, length(Ms)),
        %% 前 3 条仍是正序前缀
        ?assertEqual([{<<"a.txt">>, 2}, {<<"a.txt">>, 4}, {<<"b.txt">>, 1}],
                     [{basename(M), maps:get(line_number, M)} || M <- Ms])
    end).

no_match_returns_empty(Dir) ->
    ?_assertEqual([], grep(Dir, <<"zzzznope">>, 100)).

single_file_line_order(Dir) ->
    %% 直接指向单文件
    ?_assertEqual([1, 2, 3],
                  [maps:get(line_number, M)
                   || M <- grep_path(list_to_binary(filename:join(Dir, "b.txt")),
                                     <<"match">>, 100)]).

%%====================================================================
%% 辅助
%%====================================================================

test_dir() ->
    filename:join("/tmp", "beamai_grep_test_" ++ os:getpid()).

grep(Dir, Pattern, Max) ->
    grep_path(list_to_binary(Dir), Pattern, Max).

grep_path(Path, Pattern, Max) ->
    {ok, #{matches := Ms}} =
        beamai_tool_file:handle_grep(
          #{<<"pattern">> => Pattern, <<"path">> => Path,
            <<"max_results">> => Max}, #{}),
    Ms.

basename(M) ->
    list_to_binary(filename:basename(binary_to_list(maps:get(file, M)))).

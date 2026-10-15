#!/usr/bin/env escript
%%! -nologo

%% Phase E3 的长期 API 门禁。
%%
%% 规则 1：公共导出相对基线是新增时，必须有 -spec 和紧邻函数的 %% @doc。
%% 规则 2：审计清单里的 test_only API 不允许超过基线；基线只可人工收缩。
%%
%% 用法：
%%   escript scripts/check_api_rules.escript <inventory.md>
%%   escript scripts/check_api_rules.escript --update-exports
%%
%% CI 先运行 xref_unused_exports.escript 生成临时 inventory，再传入本脚本。
%% 公共导出基线是“允许的旧导出集合”；删除不用改基线，新增必须过规则 1。

main(["--update-exports"]) ->
    write_exports(root_dir(), exports(root_dir()));
main([Inventory]) ->
    Root = root_dir(),
    Exports = exports(Root),
    Errors0 = new_public_api_errors(Root, Exports),
    Errors = Errors0 ++ test_only_errors(Root, Inventory),
    report(Errors);
main(_) ->
    io:put_chars(standard_error,
                 "usage: check_api_rules.escript <inventory.md> | --update-exports\n"),
    halt(2).

root_dir() ->
    Path = filename:absname(re:replace(escript:script_name(), "[^/]+$", "",
                                       [{return, list}])),
    filename:dirname(Path).

%%====================================================================
%% 规则 1：新增公共导出
%%====================================================================

new_public_api_errors(Root, Current) ->
    BaselineSet = baseline_set(Root, "designs/api/public_export_baseline.txt"),
    Sources = sources(Root),
    Forms = abstract_forms(Root),
    lists:filtermap(
      fun({Module, Fun, Arity} = FA) ->
              Source = maps:get(Module, Sources),
              case maps:is_key(FA, BaselineSet) orelse
                   maps:is_key(Module, Sources) =:= false of
                  true -> false;
                  false ->
                      case valid_new_export(Source, maps:get(Module, Forms),
                                            Fun, Arity) of
                          true -> false;
                          false ->
                              {true, io_lib:format(
                                       "new public export missing -spec or %% @doc: ~p:~p/~p",
                                       [Module, Fun, Arity])}
                      end
              end
      end, Current).

%% abstract_code 是 spec 的权威来源；注释只能从源码判断。
valid_new_export(Source, Forms, Fun, Arity) ->
    has_spec(Forms, Fun, Arity)
        andalso has_doc(Source, function_clause_lines(Forms, Fun, Arity)).

has_spec(Forms, Fun, Arity) ->
    lists:any(
      fun({attribute, _, spec, {{Name, A}, _}}) when Name =:= Fun ->
              A =:= Arity orelse is_named_arity_spec(A, Arity);
         ({attribute, _, spec, {{Name, A, _Args}, _}}) when Name =:= Fun ->
              A =:= Arity orelse is_named_arity_spec(A, Arity);
         (_) -> false
      end, Forms).

%% 老版本 abstract form 可能保留命名 arity；这里兼容而不是把 OTP 升级变成假失败。
is_named_arity_spec(Arity, Arity) -> true;
is_named_arity_spec(_, _) -> false.

%% @doc 必须挂在函数 clause 前。允许中间有空行和 -spec；遇到普通代码/前一个
%% 函数就停止，避免把别的函数或模块头的文档误认过来。
has_doc(Source, ClauseLines) ->
    Lines = string:split(read_lines(Source), "\n", all),
    lists:any(fun(LineNo) -> preceding_doc(Lines, LineNo) end, ClauseLines).

preceding_doc(Lines, LineNo) ->
    preceding_doc(Lines, LineNo - 1, none).

preceding_doc(_Lines, 0, Found) -> Found =:= doc;
preceding_doc(Lines, N, Found) ->
    case line(Lines, N) of
        "" -> preceding_doc(Lines, N - 1, Found);
        Text ->
            Condensed = string:trim(Text, both, "% \t"),
            case string:prefix(Text, "%") =/= nomatch of
                true ->
                    case string:find(Condensed, "@doc") =/= nomatch of
                        true -> preceding_doc(Lines, N - 1, doc);
                        false -> preceding_doc(Lines, N - 1, Found)
                    end;
                false ->
                    case string:prefix(Condensed, "-spec") =/= nomatch of
                        true -> preceding_doc(Lines, N - 1, Found);
                        false -> Found =:= doc
                    end
            end
    end.

line(Lines, N) ->
    case lists:nth(N, Lines) of
        Bin -> string:trim(unicode:characters_to_list(Bin, utf8))
    end.

%% abstract code 的 annotation 就是源码行号；同名/同 arity 的任一 clause 有
%% @doc 即可。
function_clause_lines(Forms, Fun, Arity) ->
    [annotation_line(Anno)
     || {function, Anno, Name, A, _Clauses} <- Forms,
        Name =:= Fun, A =:= Arity].

annotation_line({Line, _Column}) -> Line;
annotation_line(Line) when is_integer(Line) -> Line.

%%====================================================================
%% 规则 2：test-only API 基线
%%====================================================================

test_only_errors(Root, Inventory) ->
    BaselineSet = baseline_set(Root, "designs/api/test_only_api_baseline.txt"),
    Current = test_only_entries(Inventory),
    [io_lib:format("new test-only public export: ~s (add it to tests/ or make it local)",
                   [Entry])
     || Entry <- Current, not maps:is_key(split_fa(Entry), BaselineSet)].

test_only_entries(InventoryPath) ->
    {ok, Bin} = file:read_file(InventoryPath),
    Lines = string:split(binary_to_list(Bin), "\n", all),
    [Entry
     || Line <- Lines,
        string:find(Line, "| test_only |") =/= nomatch,
        string:prefix(Line, "| `") =/= nomatch,
        Entry <- [entry_from(Line)]].

entry_from(Line) ->
    NoPrefix = string:slice(Line, 3),
    End = string:str(NoPrefix, "` |"),
    string:slice(NoPrefix, 0, End - 1).

split_fa(Entry) ->
    [ModFa, Arity] = string:split(Entry, "/", trailing),
    [Mod, Fun] = string:split(ModFa, ":", trailing),
    {list_to_atom(Mod), list_to_atom(Fun), list_to_integer(Arity)}.

%%====================================================================
%% 源码/基线收集
%%====================================================================

exports(Root) ->
    lists:usort(lists:append([module_exports(Path)
                              || Path <- project_beams(Root)])).

beams(Root) ->
    filelib:wildcard(Root ++ "/_build/default/lib/*/ebin/*.beam").

%% 依赖的 beam 也在同一 _build 目录里；门禁只看 apps/*/src 里的模块。
project_beams(Root) ->
    Modules = [filename:basename(Path, ".erl")
               || Path <- filelib:wildcard(Root ++ "/apps/*/src/**/*.erl")],
    [Path || Path <- beams(Root),
             lists:member(filename:basename(Path, ".beam"), Modules)].

abstract_forms(Root) ->
    maps:from_list(
      [begin
           {ok, Bin} = file:read_file(Path),
           {ok, {Module, Chunks}} = beam_lib:chunks(Bin, [abstract_code]),
           {raw_abstract_v1, Forms} = proplists:get_value(abstract_code, Chunks),
           {Module, Forms}
       end || Path <- project_beams(Root)]).

module_exports(Path) ->
    {ok, Bin} = file:read_file(Path),
    {ok, {Module, Chunks}} = beam_lib:chunks(Bin, [abstract_code]),
    {raw_abstract_v1, Forms} = proplists:get_value(abstract_code, Chunks),
    [{Module, Fun, Arity}
     || {attribute, _, export, FAs} <- Forms,
        {Fun, Arity} <- FAs].

write_exports(Root, Exports) ->
    Path = filename:join(Root, "designs/api/public_export_baseline.txt"),
    ok = filelib:ensure_dir(Path),
    Data = [io_lib:format("~p:~p/~p~n", [M, F, A]) || {M, F, A} <- lists:sort(Exports)],
    ok = file:write_file(Path, Data),
    io:format("updated ~ts (~p exports)~n", [Path, length(Exports)]).

baseline_set(Root, Relative) ->
    Path = filename:join(Root, Relative),
    {ok, Bin} = file:read_file(Path),
    Lines = string:split(binary_to_list(Bin), "\n", all),
    maps:from_list([{split_fa(Entry), true}
                    || Entry <- Lines, Entry =/= "", string:prefix(Entry, "#") =:= nomatch]).

sources(Root) ->
    maps:from_list(
      [begin
           Module = list_to_atom(filename:basename(Path, ".erl")),
           {Module, Path}
       end || Path <- filelib:wildcard(Root ++ "/apps/*/src/**/*.erl")]).

read_lines(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

%%====================================================================
%% 输出
%%====================================================================

report([]) ->
    io:put_chars("api rules: ok\n");
report(Errors) ->
    [io:format(standard_error, "~ts~n", [E]) || E <- lists:usort(Errors)],
    io:put_chars(standard_error,
                 io_lib:format("api rules: ~p violation(s)~n", [length(lists:usort(Errors))])),
    halt(1).

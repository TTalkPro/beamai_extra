#!/usr/bin/env escript
%%! -nologo

%% 公共 API 审计（tasks/downstream_quality_gates.md Phase E / E1）。
%%
%% 根 rebar.config 已按框架库语义把 exports_not_used 移出默认检查集，所以审计要
%% 单独跑一次「含 exports_not_used」的 xref。这里不改项目配置：生成一份
%% rebar.config 副本，替换其中的 xref_checks 段，用 REBAR_CONFIG 指过去
%% （重复键在 proplists 里后者生效），跑完即删。
%%
%% 对每条 unused export 收集证据并分类：
%%
%%   otp_callback —— 项目或任一依赖里被 -callback 声明过（behaviour 回调）
%%   public_api   —— docs / README / examples / design 里出现过调用写法
%%   test_only    —— 只有 apps/*/test 引用过
%%   no_evidence  —— 无任何引用证据（删除候选，人工复核后才动）
%%
%% 分类只做证据归集，不自动删除任何导出：E2 的删除必须独立提交，并写明模块 /
%% 函数 / arity 与兼容性影响。
%%
%% 用法：
%%   escript scripts/xref_unused_exports.escript          %% 打到 stdout
%%   escript scripts/xref_unused_exports.escript out.md   %% 写到文件

-define(XREF_CHECKS,
        "{xref_checks, [exports_not_used, undefined_function_calls, "
        "undefined_functions, locals_not_used, deprecated_function_calls, "
        "deprecated_functions]}.\n").

main([OutFile]) ->
    Root = root_dir(),
    Entries = run_xref(Root),
    Ctx = collect_context(Root),
    Classified = [classify(E, Ctx) || E <- Entries],
    Markdown = render(Classified),
    case OutFile of
        undefined -> io:put_chars(Markdown);
        _ ->
            ok = filelib:ensure_dir(filename:dirname(OutFile)),
            ok = file:write_file(OutFile, unicode:characters_to_binary(Markdown))
    end;
main(_) ->
    io:put_chars(standard_error,
                 "usage: xref_unused_exports.escript [out.md]\n"),
    halt(2).

root_dir() ->
    Path = filename:absname(re:replace(escript:script_name(), "[^/]+$", "",
                                       [{return, list}])),
    case filename:dirname(Path) of
        "." -> ".";
        Dir -> Dir
    end.

%%====================================================================
%% 跑 xref
%%====================================================================

run_xref(Root) ->
    Tmp = tmp_config(Root),
    try
        Output = os:cmd("REBAR_CONFIG=" ++ Tmp ++ " rebar3 xref 2>&1"),
        parse_warnings(Output)
    after
        file:delete(Tmp)
    end.

%% 替换 rebar.config 里的 xref_checks 段（而不是追加）：追加也能生效，但留下
%% 两份语义重叠的列表，读的人会以为 xref_checks 能叠加。
tmp_config(Root) ->
    Config = filename:join(Root, "rebar.config"),
    {ok, Bin} = file:read_file(Config),
    Patched =
        case re:run(Bin, <<"\\{xref_checks,[^}]*\\}\\.">>,
                    [{capture, [0], binary}]) of
            {match, [Match]} -> string:replace(Bin, Match, ?XREF_CHECKS);
            nomatch -> <<Bin/binary, "\n", ?XREF_CHECKS>>
        end,
    Tmp = filename:join(Root, "rebar.config.audit_tmp"),
    ok = file:write_file(Tmp, Patched),
    Tmp.

parse_warnings(Output) ->
    Lines = string:split(Output, "\n", all),
    [E || Line <- Lines, {ok, E} <- [parse_warning(Line)]].

parse_warning(Line) ->
    %% apps/beamai_a2a/src/beamai_a2a_auth.erl:155: Warning: beamai_a2a_auth:add_key/1 is unused export (Xref)
    Pattern =
        "apps/([^/]+)/src/.*?:\\d+: Warning: ([a-z0-9_]+):([a-z0-9_]+)/(\\d+) is unused export",
    case re:run(Line, Pattern, [{capture, all_but_first, list}]) of
        {match, [App, Mod, Fun, Arity]} ->
            {ok, #{app => list_to_atom(App),
                   module => list_to_atom(Mod),
                   function => list_to_atom(Fun),
                   arity => list_to_integer(Arity)}};
        nomatch ->
            %% exports_not_used 之外的告警（undefined call 之类）不该被静默丢掉：
            %% 出现无法解析的行说明门禁真的有别的问题。
            case re:run(Line, ": Warning: .* is ", [{capture, none}]) of
                match ->
                    io:format(standard_error, "unparsed xref warning: ~ts~n", [Line]),
                    {error, Line};
                nomatch ->
                    nomatch
            end
    end.

%%====================================================================
%% 证据收集
%%====================================================================

collect_context(Root) ->
    Docs = read_blobs(filelib:wildcard(Root ++ "/{README*,*.md}"))
        ++ read_blobs(filelib:wildcard(Root ++ "/{docs,design,designs,info,tasks,feedbacks}/*"))
        %% app 内 README 是各 app 自己的公共 API 手册，漏掉它们会把
        %% beamai_rag:split_text/2 这类「文档里写着的 API」误判成无证据。
        ++ read_blobs(filelib:wildcard(Root ++ "/apps/*/README*.md"))
        ++ read_blobs(filelib:wildcard(Root ++ "/examples/**/*")),
    Tests = read_blobs(filelib:wildcard(Root ++ "/apps/*/test/**/*")),
    #{callbacks => callbacks(Root),
      docs => Docs,
      module_docs => module_docs(Root),
      sources => sources(Root),
      tests => Tests}.

%% 本项目的文档习惯写在模块头的 %%% 注释里（含可运行的调用示例），这些例子
%% 是「这是公共 API」的第一手证据，缺了会把 beamai_mcp_client_registry:get_client/1
%% 这类明明写着用法的函数误判成无证据。
module_docs(Root) ->
    maps:map(fun(_Mod, Bin) -> header_comment(Bin) end, sources(Root)).

sources(Root) ->
    maps:from_list(
      [case file:read_file(P) of
           {ok, B} -> {list_to_atom(filename:basename(P, ".erl")), B};
           {error, _} -> {list_to_atom(filename:basename(P, ".erl")), <<>>}
       end
       || P <- filelib:wildcard(Root ++ "/apps/*/src/**/*.erl")]).

%% 文件开头连续的注释行（%%% / %）即模块头文档。
header_comment(Bin) ->
    Lines = string:split(binary_to_list(Bin), "\n", all),
    {Kept, _Rest} =
        lists:splitwith(fun(L) ->
                            S = string:trim(L),
                            string:prefix(S, "%%") =/= nomatch
                        end, Lines),
    iolist_to_binary([unicode:characters_to_binary(L, unicode, utf8) || L <- Kept]).

read_blobs(Paths) ->
    [begin
         case file:read_file(P) of
             {ok, B} -> B;
             {error, _} -> <<>>
         end
     end
     || P <- Paths,
        lists:member(filename:extension(P), [".md", ".erl", ".hrl"])].

%% 扫已编译 beam 的 abstract_code，抽 -callback 声明。behaviour 回调在项目内部
%% 「无人调用」是设计使然（框架按约定回调宿主实现），必须与真死代码分开。
callbacks(Root) ->
    Beams = filelib:wildcard(Root ++ "/_build/default/lib/*/ebin/*.beam")
        ++ filelib:wildcard(Root ++ "/_build/default/lib/*/apps/*/ebin/*.beam")
        ++ filelib:wildcard(Root ++ "/_build/default/checkouts/*/ebin/*.beam")
        ++ otp_beams(),
    lists:foldl(fun(Bea, Acc) -> add_callbacks(Bea, Acc) end, #{}, Beams).

%% gen_server / supervisor / proc_lib 的回调声明在 stdlib、kernel 里，
%% 不扫 OTP 目录的话这些回调永远不会被认出来。
otp_beams() ->
    lists:append(
      [filelib:wildcard(filename:join(code:lib_dir(App), "ebin/*.beam"))
       || App <- [stdlib, kernel, sasl]]).

add_callbacks(Beam, Acc) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            %% raw 形式里属性是 4 元组 {attribute, Anno, Name, Value}。
            Specs = [FA || {attribute, _, callback, Spec} <- Forms,
                           FA <- callback_fa(Spec)],
            maps:merge(Acc, maps:from_list(Specs));
        _ ->
            Acc
    end.

%% beam_lib 给的是 core Erlang 形式的 spec（不是 AST）：回调是
%% `{{Name, Arity}, Opts}`。两种形状都兜住，免得换 OTP 版本就静默归零。
callback_fa({{Name, Arity}, _Opts}) when is_atom(Name), is_integer(Arity) ->
    [{Name, Arity}];
callback_fa({Name, Arity, _Args}) when is_atom(Name), is_integer(Arity) ->
    [{Name, Arity}];
callback_fa(_) ->
    [].

%%====================================================================
%% 分类
%%====================================================================

classify(Entry, Ctx) ->
    #{function := Fun, arity := Arity, module := Mod} = Entry,
    case maps:is_key({Fun, Arity}, maps:get(callbacks, Ctx)) of
        true ->
            Entry#{class => otp_callback, evidence => "项目或依赖里被 -callback 声明"};
        false ->
            case statem_state_function(maps:get(sources, Ctx), Mod, Fun, Arity) of
                true ->
                    Entry#{class => otp_callback,
                           evidence => "gen_statem 状态函数（由状态机分派）"};
                false ->
                    case module_doc_evidence(Ctx, Mod, Fun) of
                        none ->
                            classify_docs(Entry, Ctx, Fun, Mod);
                        _ ->
                            Entry#{class => public_api,
                                   evidence => "模块头 %%% 文档里有调用示例"}
                    end
            end
    end.

%% gen_statem 的状态函数由 OTP 的状态机循环分派，项目里没有任何一处会写
%% `connected(...)` 这样的调用，xref 因此把它们全报成 unused export——
%% 那是误报，不是死代码。判据：模块声明了 -behaviour(gen_statem)，且函数名
%% 在本模块里作为状态名出现过（next_state / keep_state / repeat 的第二个元素）。
statem_state_function(Sources, Mod, Fun, Arity) ->
    Arity >= 2
        andalso
        case maps:find(Mod, Sources) of
            {ok, Bin} ->
                Text = binary_to_list(Bin),
                declares_gen_statem(Text)
                    andalso state_name_used(Text, atom_to_list(Fun));
            error ->
                false
        end.

declares_gen_statem(Text) ->
    string:find(Text, "-behaviour(gen_statem)") =/= nomatch.

state_name_used(Text, Name) ->
    lists:any(fun(Tag) ->
                  Pattern = "{" ++ Tag ++ ", " ++ Name ++ ",",
                  string:find(Text, Pattern) =/= nomatch
              end, ["next_state", "keep_state", "repeat"]).

classify_docs(Entry, Ctx, Fun, Mod) ->
    case evidence(maps:get(docs, Ctx), Mod, Fun) of
        none ->
            case evidence(maps:get(tests, Ctx), Mod, Fun) of
                none ->
                    case called_in_own_module(maps:get(sources, Ctx), Mod, Fun) of
                        true ->
                            Entry#{class => internal_only,
                                   evidence => "仅本模块内部调用"};
                        false ->
                            Entry#{class => no_evidence, evidence => ""}
                    end;
                Level ->
                    Entry#{class => test_only, evidence => test_evidence(Level)}
            end;
        Level ->
            Entry#{class => public_api, evidence => doc_evidence(Level)}
    end.

%% xref 的 exports_not_used 只看**跨模块**调用：本模块内部调用不算使用。
%% 这一类不是死代码，动作是去掉 export 收缩成局部函数（E2 的第二条），
%% 与「删除」必须分开。判定放宽：只看本模块里有没有裸 `fun(` 出现，
%% 注释行不算。
called_in_own_module(Sources, Mod, Fun) ->
    case maps:find(Mod, Sources) of
        {ok, Bin} ->
            Name = atom_to_list(Fun) ++ "(",
            Spec = "-spec " ++ atom_to_list(Fun) ++ "(",
            Lines = string:split(binary_to_list(Bin), "\n", all),
            lists:any(
              fun(Line) ->
                  Trimmed = string:trim(Line),
                  %% 排除注释行、函数自身定义头与 -spec 行，否则每个函数都会
                  %% 被自己的定义行判成「内部有调用」。定义头以 `->` 收尾，
                  %% 调用以 `;` / `,` 收尾——这一步就是靠这个区别。
                  string:prefix(Trimmed, "%") =:= nomatch
                      andalso not is_definition_head(Trimmed, Name)
                      andalso string:prefix(Trimmed, Spec) =:= nomatch
                      andalso occurs(list_to_binary(Trimmed), Name)
              end, Lines);
        error ->
            false
    end.

is_definition_head(Trimmed, Name) ->
    string:prefix(Trimmed, Name) =/= nomatch
        andalso string:find(Trimmed, "->") =/= nomatch.

module_doc_evidence(Ctx, Mod, Fun) ->
    case maps:find(Mod, maps:get(module_docs, Ctx)) of
        {ok, Blob} -> evidence([Blob], Mod, Fun);
        error -> none
    end.

%% 证据强度分两档：`mod:fun(` 是可靠证据；裸 `fun(` 只说明**同名**函数在别处
%% 被调用过，可能是完全无关的同名函数（`close(` / `send(` 这类到处都是），
%% 所以单列一档提醒复核。
evidence(Blobs, Mod, Fun) ->
    Name = atom_to_list(Fun),
    case has_occurrence(Blobs, atom_to_list(Mod) ++ ":" ++ Name ++ "(") of
        true -> qualified;
        false ->
            case has_occurrence(Blobs, Name ++ "(") of
                true -> same_name;
                false -> none
            end
    end.

doc_evidence(qualified) -> "docs/examples 出现 mod:fun( 调用";
doc_evidence(same_name) -> "docs/examples 只出现同名 fun( 调用，需复核".

test_evidence(qualified) -> "apps/*/test 出现 mod:fun( 调用";
test_evidence(same_name) -> "apps/*/test 只出现同名 fun( 调用，需复核".

has_occurrence(Blobs, Needle) ->
    lists:any(fun(Blob) -> occurs(Blob, Needle) end, Blobs).

%% binary:compile_pattern/1 是**字面量**匹配（不做正则），所以这里自己判断
%% 词边界：名字前面不能紧跟字母 / 数字 / 下划线，否则 `run(` 会顺带命中
%% `rerun(`。名字后面不判——调用写法一律带 `(`。
occurs(Blob, Needle) ->
    search(Blob, unicode:characters_to_binary(Needle, unicode, utf8), 0).

search(Blob, Needle, From) ->
    Rest = binary:part(Blob, From, byte_size(Blob) - From),
    case binary:match(Rest, Needle) of
        nomatch ->
            false;
        {Pos, _Len} ->
            Abs = From + Pos,
            boundary(Blob, Abs) orelse search(Blob, Needle, Abs + byte_size(Needle))
    end.

boundary(_Blob, 0) ->
    true;
boundary(Blob, Pos) ->
    case binary:at(Blob, Pos - 1) of
        C when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z);
               (C >= $0 andalso C =< $9); C =:= $_ ->
            false;
        _ ->
            true
    end.

%%====================================================================
%% 渲染
%%====================================================================

render(Entries) ->
    Apps = lists:usort([maps:get(app, E) || E <- Entries]),
    Counts = count_classes(Entries),
    Summary = io_lib:format(
                "beamai_extra unused exports，按 app 分组并附证据分类。~n"
                "~n"
                "生成方式：`escript scripts/xref_unused_exports.escript`。~n"
                "分类只是证据归集，**不代表可以删**：no_evidence 一列是候选，~n"
                "删除必须人工复核后独立提交。~n"
                "~n"
                "总计 ~p 条：~ts~n~n",
                [length(Entries),
                 string:join([io_lib:format("~ts ~p", [C, N])
                              || {C, N} <- lists:sort(maps:to_list(Counts))], " / ")]),
    [Summary] ++ [render_app(App, [E || E <- Entries, maps:get(app, E) =:= App])
                  || App <- Apps] ++
    ["\n证据列给出的是**唯一**命中位置；同名函数可能误命中，"
     "复核以调用写法（是否真指向本模块）为准。\n"].

render_app(App, Entries) ->
    Sorted = lists:sort(fun(A, B) ->
                           {maps:get(module, A), maps:get(function, A)} =<
                           {maps:get(module, B), maps:get(function, B)}
                       end, Entries),
    [io_lib:format("## ~p（~p 条）\n\n", [App, length(Sorted)]),
     "| module:function/arity | 分类 | 证据 |\n|---|---|---|\n"] ++
    [io_lib:format("| `~p:~p/~p` | ~s | ~ts |~n",
                   [maps:get(module, E), maps:get(function, E),
                    maps:get(arity, E), maps:get(class, E),
                    maps:get(evidence, E)])
     || E <- Sorted] ++
    ["\n"].

count_classes(Entries) ->
    lists:foldl(fun(E, Acc) ->
                    maps:update_with(maps:get(class, E), fun(N) -> N + 1 end, 1, Acc)
                end, #{}, Entries).

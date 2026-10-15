%%%-------------------------------------------------------------------
%%% @doc 流式 JSON 的「已经写完的那一段」—— activity 插件的中间态靠它
%%%
%%% 模型流式吐出来的工具参数是**半截 JSON**（`TOOL_CALL_ARGS` 的 delta 逐字拼起来
%%% 就是它）。`beamai_agui_plugin` 攒到 `TOOL_CALL_END` 再一次性 `json:decode/1`，
%%% 于是 activity 只在参数到齐时出现一次 —— 「边生成边长出来」就没有了。
%%%
%%% 解法不是写一个完整的增量 JSON 解析器，而是**找切点**：扫一遍这半截文本，记下
%%% 「最近一个刚写完的值」的位置，把它之后的半截全丢掉，剩下的补上闭合括号就是
%%% 一份合法 JSON。
%%%
%%% ```
%%% 半截文本                        切点            补齐后
%%% {"surfaceId":"s","components":[  值 "s" 之后   {"surfaceId":"s"}
%%% {"id":"root"},"b":1
%%%                        ───────▶  {"surfaceId":"s","components":[{"id":"root"}]}
%%% ```
%%%
%%% 切点只在「**一个值刚写完**」的地方取，所以切出来的前缀永远是
%%% 「若干个完整值 + 若干个没写完的容器」，补括号即合法。两个容易踩的坑：
%%%
%%% 1. **数字不能在中间切**：`12` 写了一半是 `1`，切在前者会得到一个错的数。
%%%    所以数字 / `true` / `false` / `null` 都要等**分隔符**到了才算完；
%%% 2. **字符串里的括号不算数**：`"text": "a{b\"c"` 里的 `{` 不能进栈。转义
%%%    (`\"`) 也不能当成结束的引号。
%%%
%%% 切不出来（第一个值都还没写完）就返回 `incomplete` —— 那不是错误，只是现在还
%%% 没有中间态可发。
%%%
%%% == 为什么不自己写 parser ==
%%%
%%% 增量 JSON 解析难在**撤销**（值可能继续长）与**容错**（模型可能吐出非法字符）。
%%% 切点法把这两件事都交给现成的 `json:decode/1`：我们只负责找一个「一定能解」的
%%% 位置，解不动就不发。宁可少发一条中间态，也不发一个错的。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_json_prefix).

-export([complete_prefix/1, parse_prefix/1]).

%% 容器：object 的下一格可能是键，array 的下一格一定是值
-type frame() :: object | array.

%%====================================================================
% API
%%====================================================================

%% @doc 半截 JSON 里「已经写完的那一段」，补齐括号，可以直接 `json:decode/1`
%%
%% 一个完整值都还没写完时返回 `incomplete`。
-spec complete_prefix(binary()) -> {ok, binary()} | incomplete.
complete_prefix(Bin) when is_binary(Bin) ->
    case scan(Bin, 0, [], value, none) of
        none ->
            incomplete;
        {Pos, Stack} when Pos > 0 ->
            {ok, <<(binary:part(Bin, 0, Pos))/binary, (closers(Stack))/binary>>}
    end.

%% @doc `complete_prefix/1` + 解码，一步到位
%%
%% 解不出来（模型吐了非法 JSON）返回 `{error, Reason}`，由调用方决定退到什么
%% 行为 —— 一般是当这次没有中间态。
-spec parse_prefix(binary()) -> {ok, term()} | {error, term()} | incomplete.
parse_prefix(Bin) when is_binary(Bin) ->
    case complete_prefix(Bin) of
        incomplete ->
            incomplete;
        {ok, Prefix} ->
            try {ok, json:decode(Prefix)}
            catch Class:Reason -> {error, {Class, Reason}}
            end
    end.

%%====================================================================
%% 内部 - 扫描
%%====================================================================

%% 状态：位置 / 容器栈 / 当前这个格子在等什么 / 最近一个完整值的位置
%%
%% `Safe` 是 `{Pos, Stack}` —— **栈要连着切点一起存**：补的括号是那个时刻的容器
%% 栈决定的，不是扫完时的（`[{"a":1` 该补 `}]`，不是 `}]`）。
-spec scan(binary(), non_neg_integer(), [frame()], atom(),
           none | {non_neg_integer(), [frame()]}) ->
    none | {non_neg_integer(), [frame()]}.
scan(Bin, Pos, _Stack, _Expect, Safe) when Pos >= byte_size(Bin) ->
    Safe;
scan(Bin, Pos, Stack, Expect, Safe) ->
    case binary:at(Bin, Pos) of
        C when C =:= $\s; C =:= $\n; C =:= $\r; C =:= $\t ->
            scan(Bin, Pos + 1, Stack, Expect, Safe);
        $" ->
            scan_string(Bin, Pos, Stack, Expect, Safe);
        ${ ->
            scan(Bin, Pos + 1, [object | Stack], key, Safe);
        $[ ->
            scan(Bin, Pos + 1, [array | Stack], value, Safe);
        $} ->
            scan_closer(Bin, Pos, Stack, Safe);
        $] ->
            scan_closer(Bin, Pos, Stack, Safe);
        $: ->
            scan(Bin, Pos + 1, Stack, value, Safe);
        $, ->
            scan(Bin, Pos + 1, Stack, next_slot(Stack), comma_safe(Safe, Pos, Stack));
        C when C =:= $-; C >= $0, C =< $9 ->
            scan_number(Bin, Pos, Stack, Safe);
        C when C =:= $t; C =:= $f; C =:= $n ->
            scan_literal(Bin, Pos, Stack, Expect, Safe);
        %% 认不出来的字符：后面怎么写都不会合法了，就此打住
        _Other ->
            Safe
    end.

%% 字符串：`"` 里的引号不算结束
scan_string(Bin, Pos, Stack, Expect, Safe) ->
    case string_end(Bin, Pos + 1, false) of
        nomatch ->
            Safe;   %% 引号还没写完
        End ->
            case Expect of
                key -> scan(Bin, End + 1, Stack, colon, Safe);
                _Value -> scan(Bin, End + 1, Stack, next, {End + 1, Stack})
            end
    end.

string_end(Bin, Pos, _Escaped) when Pos >= byte_size(Bin) ->
    nomatch;
string_end(Bin, Pos, true) ->
    string_end(Bin, Pos + 1, false);
string_end(Bin, Pos, false) ->
    case binary:at(Bin, Pos) of
        $\\ -> string_end(Bin, Pos + 1, true);
        $" -> Pos;
        _Other -> string_end(Bin, Pos + 1, false)
    end.

%% 逗号只是分隔符，不能跟着切点一起留下；否则补括号后会变成非法的尾逗号。
comma_safe(none, _Pos, _Stack) ->
    none;
comma_safe(_OldSafe, Pos, Stack) ->
    {Pos, Stack}.

%% 数字：等到分隔符才算写完（见模块头「数字不能在中间切」）
scan_number(Bin, Pos, Stack, Safe) ->
    case number_end(Bin, Pos + 1) of
        {End, Delim} ->
            scan(Bin, Delim + 1, Stack, next, {End, Stack});
        truncated ->
            Safe
    end.

number_end(Bin, Pos) when Pos >= byte_size(Bin) ->
    truncated;
number_end(Bin, Pos) ->
    case binary:at(Bin, Pos) of
        C when C =:= $-; C =:= $+; C =:= $.; C =:= $e; C =:= $E ->
            number_end(Bin, Pos + 1);
        C when C >= $0, C =< $9 ->
            number_end(Bin, Pos + 1);
        C when C =:= $,; C =:= $}; C =:= $]; C =:= $\s; C =:= $\n; C =:= $\r; C =:= $\t ->
            {Pos, Pos};
        _Other ->
            truncated
    end.

%% `true` / `false` / `null`：同样等分隔符
scan_literal(Bin, Pos, Stack, Expect, Safe) ->
    case literal_end(Bin, Pos + 1) of
        {End, Delim} when Expect =:= key ->
            scan(Bin, Delim + 1, Stack, next_slot(Stack), {End, Stack});
        {End, Delim} ->
            scan(Bin, Delim + 1, Stack, next, {End, Stack});
        truncated ->
            Safe
    end.

literal_end(Bin, Pos) when Pos >= byte_size(Bin) ->
    truncated;
literal_end(Bin, Pos) ->
    case binary:at(Bin, Pos) of
        C when C >= $a, C =< $z ->
            literal_end(Bin, Pos + 1);
        C when C =:= $,; C =:= $}; C =:= $]; C =:= $\s; C =:= $\n; C =:= $\r; C =:= $\t ->
            {Pos, Pos};
        _Other ->
            truncated
    end.

%% 收尾括号：**它自己那个容器已经写完了**，栈要弹掉 —— 补的括号是外层的
%% （`[{"a":1` 补 `}]`，不是 `}]`）。注意这个位置永远是个安全切点。
scan_closer(Bin, Pos, [_Closed | Parent], _Safe) ->
    scan(Bin, Pos + 1, Parent, next, {Pos + 1, Parent});
scan_closer(_Bin, _Pos, [], Safe) ->
    Safe.

%%====================================================================
%% 内部 - 状态迁移
%%====================================================================

%% 逗号之后：对象等下一个键，数组等下一个值
next_slot([object | _]) -> key;
next_slot([array | _]) -> value;
next_slot([]) -> done.

closers(Stack) ->
    %% 栈是 top -> outer（`[array, object]` = 数组还没闭合，外层对象也没闭合），
    %% 所以关闭括号必须按现有顺序输出：`]}`
    iolist_to_binary([case F of object -> $}; array -> $] end || F <- Stack]).

%%%-------------------------------------------------------------------
%%% @doc state 槽的 JSON Patch 差分（RFC 6902 子集）
%%%
%%% AG-UI 的 `STATE_DELTA.delta` 是一个 **JSON Patch 数组**，不是"改了哪些键"的
%%% map —— 前端拿它对本地状态做原地增量修改。这里负责把「上一份 state」与
%%% 「这一份 state」之间的差算成那个数组。
%%%
%%% == 只产三种 op ==
%%%
%%% `add` / `remove` / `replace`。RFC 还有 `move` / `copy` / `test`，但那三种要么
%%% 需要识别"同一个值挪了位置"（对状态槽没意义，还容易算错），要么是断言。
%%% 三种 op 足以表达任意两份 JSON 之间的差。
%%%
%%% == 只对 map 递归 ==
%%%
%%% 两边都是 map 才逐键往下比；数组和标量一律整体 `replace`。数组的元素级 diff
%%% 需要一套 LCS 才不会算出一堆无意义的移位补丁，收益不抵复杂度 —— 状态槽里的
%%% 数组通常是"整体换一份"的语义（如一个列表被工具重算）。
%%%
%%% == 路径转义 ==
%%%
%%% JSON Pointer 里 `~` 与 `/` 有特殊含义，必须转义成 `~0` / `~1`（RFC 6901）。
%%% 状态槽的键是用户给的，含斜杠完全可能 —— 不转义会算出一条指向别处的路径。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_state).

-export([diff/2, escape/1]).

-export_type([patch/0, op/0]).

-type op() :: #{binary() => term()}.
-type patch() :: [op()].

%%====================================================================
%% API
%%====================================================================

%% @doc 两份 state 之间的 JSON Patch
%%
%% 键序固定（按键排序），故同样的输入总得到同样的补丁 —— 便于测试与比对。
-spec diff(term(), term()) -> patch().
diff(Old, New) ->
    lists:reverse(diff(Old, New, <<>>, [])).

%% @doc JSON Pointer 的单段转义（RFC 6901：`~` → `~0`，`/` → `~1`）
-spec escape(binary()) -> binary().
escape(Segment) ->
    binary:replace(binary:replace(Segment, <<"~">>, <<"~0">>, [global]),
                   <<"/">>, <<"~1">>, [global]).

%%====================================================================
%% 内部
%%====================================================================

diff(Same, Same, _Path, Acc) ->
    Acc;
diff(Old, New, Path, Acc) when is_map(Old), is_map(New) ->
    Keys = lists:usort(keys(Old) ++ keys(New)),
    lists:foldl(fun(Key, A) -> diff_key(Key, Old, New, Path, A) end, Acc, Keys);
diff(_Old, New, <<>>, Acc) ->
    %% 根本身换了型（map → 标量等）：整份替换
    [op(<<"replace">>, <<"/">>, New) | Acc];
diff(_Old, New, Path, Acc) ->
    [op(<<"replace">>, Path, New) | Acc].

diff_key(Key, Old, New, Path, Acc) ->
    Child = <<Path/binary, "/", (escape(Key))/binary>>,
    case {find(Key, Old), find(Key, New)} of
        {{ok, V}, {ok, V}} -> Acc;
        {error, {ok, V}} -> [op(<<"add">>, Child, V) | Acc];
        {{ok, _}, error} -> [op(<<"remove">>, Child) | Acc];
        {{ok, OldV}, {ok, NewV}} -> diff(OldV, NewV, Child, Acc)
    end.

%% state 槽的键约定是 binary，但嵌套值里出现 atom 键完全可能（工具随手写的 map），
%% 两者都要能取到、也都要能转成路径段
keys(Map) ->
    [to_key(K) || K <- maps:keys(Map)].

to_key(K) when is_binary(K) -> K;
to_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
to_key(K) when is_integer(K) -> integer_to_binary(K);
to_key(K) -> iolist_to_binary(io_lib:format("~p", [K])).

%% 按 binary 键找，找不到再按同名 atom 找（写入方用哪种都认）
find(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, _} = Found ->
            Found;
        error ->
            try binary_to_existing_atom(Key, utf8) of
                Atom -> maps:find(Atom, Map)
            catch
                error:badarg -> error
            end
    end.

op(Op, Path) ->
    #{<<"op">> => Op, <<"path">> => Path}.

op(Op, Path, Value) ->
    #{<<"op">> => Op, <<"path">> => Path, <<"value">> => Value}.

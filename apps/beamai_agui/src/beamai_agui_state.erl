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

-export([diff/2, apply_patch/2, escape/1, unescape/1]).

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

%% @doc 把一串补丁打到一份 state 上（`diff/2` 的逆）
%%
%% 为什么需要它：会话层要维护"这个 thread 现在的 state 是什么"，而流上跑的是
%% `STATE_SNAPSHOT` 立基线 + 一串 `STATE_DELTA`。没有 apply 就只能记住最后一次
%% 快照 —— 那份在第一个补丁之后就过期了，`/threads/:id/state` 只好撒谎。
%%
%% **只认自己产得出的三种 op**（`add` / `remove` / `replace`），别的原样跳过 ——
%% 补丁也可能来自宿主自己发的 `STATE_DELTA`，一条不认识的 op 不该让整份 state 作废。
%% 路径指向不存在的父节点同样跳过：宁可少改一处，也不要凭空造出一棵中间树。
-spec apply_patch(term(), patch()) -> term().
apply_patch(State, Ops) when is_list(Ops) ->
    lists:foldl(fun apply_op/2, State, Ops);
apply_patch(State, _) ->
    State.

%% @doc JSON Pointer 的单段转义（RFC 6901：`~` → `~0`，`/` → `~1`）
-spec escape(binary()) -> binary().
escape(Segment) ->
    binary:replace(binary:replace(Segment, <<"~">>, <<"~0">>, [global]),
                   <<"/">>, <<"~1">>, [global]).

%% @doc `escape/1` 的逆（**`~1` 必须先解**：先解 `~0` 会把 `~01` 变成 `~1` 再变成
%% `/`，而它本该是字面量 `~1`）
-spec unescape(binary()) -> binary().
unescape(Segment) ->
    binary:replace(binary:replace(Segment, <<"~1">>, <<"/">>, [global]),
                   <<"~0">>, <<"~">>, [global]).

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

%%====================================================================
%% 内部 - 打补丁
%%====================================================================

%% @private 根整体替换：与 diff/3 的根分支对称（那条产的就是 path `/`）
apply_op(#{<<"op">> := <<"replace">>, <<"path">> := <<"/">>, <<"value">> := V}, _State) ->
    V;
apply_op(#{<<"op">> := Op, <<"path">> := Path} = PatchOp, State) ->
    case segments(Path) of
        [] ->
            State;
        Segments ->
            update(State, Segments, Op, maps:get(<<"value">>, PatchOp, undefined))
    end;
apply_op(_Unknown, State) ->
    State.

%% @private 路径拆成段（去掉前导 `/`，逐段反转义）
segments(<<"/", Rest/binary>>) ->
    [unescape(S) || S <- binary:split(Rest, <<"/">>, [global])];
segments(<<>>) ->
    [];
segments(Path) when is_binary(Path) ->
    %% 不以 `/` 开头不是合法 JSON Pointer，但宽容地当作单段处理
    [unescape(Path)];
segments(_) ->
    [].

%% @private 沿路径下钻到父节点再改那一格
update(Map, [Key], Op, Value) when is_map(Map) ->
    case Op of
        <<"remove">> -> del(Key, Map);
        <<"add">> -> put(Key, Value, Map);
        <<"replace">> -> put(Key, Value, Map);
        _ -> Map
    end;
update(Map, [Key | Rest], Op, Value) when is_map(Map) ->
    case find(Key, Map) of
        {ok, Child} when is_map(Child) ->
            put(Key, update(Child, Rest, Op, Value), Map);
        _ ->
            %% 父节点不存在或不是 map：跳过（见 apply_patch/2 的模块说明）
            Map
    end;
update(Other, _Segments, _Op, _Value) ->
    Other.

%% @private 写一格：既有键是什么形态就沿用什么形态（atom 键的 map 不该被写成两份）
put(Key, Value, Map) ->
    Map2 = maps:remove(Key, Map),
    case existing_atom_key(Key, Map) of
        {ok, Atom} -> (maps:remove(Atom, Map2))#{Atom => Value};
        error -> Map2#{Key => Value}
    end.

%% @private 删一格（binary 与同名 atom 两种形态都删）
del(Key, Map) ->
    Map2 = maps:remove(Key, Map),
    case existing_atom_key(Key, Map) of
        {ok, Atom} -> maps:remove(Atom, Map2);
        error -> Map2
    end.

%% @private 这张 map 里有没有与该 binary 键同名的 atom 键
existing_atom_key(Key, Map) ->
    case maps:is_key(Key, Map) of
        true ->
            error;
        false ->
            try binary_to_existing_atom(Key, utf8) of
                Atom -> case maps:is_key(Atom, Map) of
                            true -> {ok, Atom};
                            false -> error
                        end
            catch
                error:badarg -> error
            end
    end.

%%%-------------------------------------------------------------------
%%% @doc AG-UI 协议类型：RunAgentInput 的解析与访问
%%%
%%% 线上格式是 camelCase 的 JSON（`threadId` / `runId` / `forwardedProps`），
%%% 解出来存成 snake_case 的 atom 键 map —— 与 beamai 其余部分一致。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_types).

-export([parse_run_input/1]).
-export([thread_id/1, run_id/1, parent_run_id/1, messages/1, tools/1, state/1]).
-export([resume/1, forwarded_props/1]).
-export([agent_name/1, last_user_message/1, resume_input/2]).
-export([context_prompt/1, state_prompt/1, content_to_text/1,
         frontend_tool_names/1]).

-export_type([run_input/0]).

-type run_input() :: #{
    thread_id := binary(),
    run_id := binary(),
    parent_run_id := binary() | undefined,
    messages := [map()],
    tools := [map()],
    context := [map()],
    resume := [map()],
    state := term(),
    forwarded_props := map()
}.

%%====================================================================
%% 解析
%%====================================================================

%% @doc 解析 POST body（JSON binary 或已解出的 map）
%%
%% threadId / runId 缺省时自动生成：AG-UI 允许客户端不带 —— 首轮对话本来就
%% 没有 thread。生成的 id 会经 RUN_STARTED 回给客户端，后续请求带回来即可续上。
-spec parse_run_input(binary() | map()) -> {ok, run_input()} | {error, term()}.
parse_run_input(Body) ->
    case beamai_utils:parse_json(Body) of
        Map when map_size(Map) =:= 0 ->
            {error, invalid_body};
        Map ->
            {ok, #{
                thread_id => get_id(Map, <<"threadId">>, <<"thread">>),
                run_id => get_id(Map, <<"runId">>, <<"run">>),
                parent_run_id => maps:get(<<"parentRunId">>, Map, undefined),
                messages => get_list(Map, <<"messages">>),
                tools => get_list(Map, <<"tools">>),
                context => get_list(Map, <<"context">>),
                resume => get_list(Map, <<"resume">>),
                state => maps:get(<<"state">>, Map, #{}),
                forwarded_props => get_map(Map, <<"forwardedProps">>)
            }}
    end.

%%====================================================================
%% 访问器
%%====================================================================

-spec thread_id(run_input()) -> binary().
thread_id(#{thread_id := V}) -> V.

-spec run_id(run_input()) -> binary().
run_id(#{run_id := V}) -> V.

%% @doc 调用方声明的父 run（嵌套 run）。**我们不解释它** —— 不建父子索引、
%% 不做级联停止，只让它出现在 `RUN_STARTED.parentRunId` 上，客户端据此把这条
%% run 挂回父链。协议里它是可选的，没有就没有。
-spec parent_run_id(run_input()) -> binary() | undefined.
parent_run_id(#{parent_run_id := V}) when is_binary(V) -> V;
parent_run_id(_) -> undefined.

-spec messages(run_input()) -> [map()].
messages(#{messages := V}) -> V.

-spec tools(run_input()) -> [map()].
tools(#{tools := V}) -> V.

-spec state(run_input()) -> term().
state(#{state := V}) -> V.

-spec forwarded_props(run_input()) -> map().
forwarded_props(#{forwarded_props := V}) -> V.

%% @private 前端注册的本轮环境说明（`useAgentContext`）
-spec context(run_input()) -> [map()].
context(#{context := V}) -> V;
context(_) -> [].

%% @doc interrupt 协议的答复数组（`RunAgentInput.resume[]`）
-spec resume(run_input()) -> [map()].
resume(#{resume := V}) -> V;
resume(_) -> [].

%% @doc `RunAgentInput.tools` 里那些工具的名字
%%
%% 这些是**前端定义、前端执行**的工具（服务端没有实现）。认出它们要紧：模型调到
%% 一个前端工具时，暂停之后该回来的是**这次调用的结果**，而不是一个 approve /
%% reject 的决定 —— 两种 interrupt 要的东西不一样，`responseSchema` 也就不一样。
%% 客户端每次请求都重发 `tools`，所以续跑那一轮照样算得出来。
-spec frontend_tool_names(run_input()) -> [binary()].
frontend_tool_names(Input) ->
    [N || T <- tools(Input), is_map(T), (N = tool_name(T)) =/= undefined].

tool_name(Tool) ->
    case first_of(Tool, [<<"name">>, name]) of
        Name when is_binary(Name), Name =/= <<>> -> Name;
        _ -> undefined
    end.

%% @doc 要跑哪个 agent：约定放在 forwardedProps.agent（AG-UI 协议本身不带这个
%% 字段——端点多以路径区分 agent；两条路都留着，路径优先由适配器给出）
-spec agent_name(run_input()) -> binary() | undefined.
agent_name(Input) ->
    case maps:get(<<"agent">>, forwarded_props(Input), undefined) of
        Name when is_binary(Name) -> Name;
        _ -> undefined
    end.

%% @doc 取最后一条 user 消息的文本
%%
%% beamai_agent:run/stream 每次只收**一条**新消息，跨轮历史由 memory filter 按
%% conversation_id 维护（见上游 docs/MEMORY.md）。而 AG-UI 客户端每次会把整个
%% messages 数组发上来。两边对接的最小正确做法就是只取最后一条 user 消息，
%% 让历史归 memory 管——否则历史会被重复计入两次。
%%
%% 客户端与服务端历史不一致时（换设备、清缓存）由 memory provider 决定谁赢，
%% 这属于 thread 持久化那一层的事，不在骨架里做。
-spec last_user_message(run_input()) -> {ok, binary()} | {error, no_user_message}.
last_user_message(Input) ->
    case lists:reverse([M || M <- messages(Input), is_user(M)]) of
        [Last | _] -> {ok, content_of(Last)};
        [] -> {error, no_user_message}
    end.

%% @doc 从请求里解出「恢复一次暂停」的决策
%%
%% 四条来路，按优先级：
%%
%% 1. `resume[]` —— **AG-UI interrupt 协议的正路**：上一条 run 以
%%    `RUN_FINISHED + outcome:"interrupt"` 收口，客户端把人的决定放在下一次 run 的
%%    请求体里带回来（`{interruptId, status|decision, payload}`）。协议里没有单独的
%%    审批端点，就是这一条。
%% 2. `forwardedProps.resume` = `#{"decision" => D, "payload" => P}` —— 我们自己的
%%    扩展位，先于协议实现，留着不动（老前端还在用）。
%% 3. 最后一条 `role=tool` 且 `toolCallId` 对得上被中断调用的消息 —— 前端工具
%%    执行完，结果作为 tool 消息回传。映射成 `reply` 语义（工具不再由服务端执行，
%%    这条内容即结果）。
%% 4. 最后一条 user 消息 —— 聊天式 HITL：agent 停下来问，人直接回一句话。
%%    走上游的「自由文本作结果」回落。
%%
%% 四条都没有则 none —— 一个 thread 卡在暂停上时，不带任何输入的请求没法推进。
-spec resume_input(run_input(), binary() | undefined) ->
    {ok, term(), map()} | none.
resume_input(Input, PendingToolCallId) ->
    case protocol_resume(Input, PendingToolCallId) of
        {ok, _, _} = Protocol ->
            Protocol;
        none ->
            case explicit_resume(Input) of
                {ok, _D, _P} = Explicit -> Explicit;
                none -> implicit_resume(Input, PendingToolCallId)
            end
    end.

%% @doc AG-UI `RunAgentInput.context` → 一段 system 文本；这轮没有上下文则 undefined
%%
%% 条目是 `#{description, value}`。前端 `useAgentContext` 注册，客户端**每次 run
%% 都重发全量** —— 所以它天然是 turn 级的：这一轮带就有，下一轮不带就没了。
%%
%% 开头那句抬头是给模型的**出处说明**：不写的话这几行会被当成用户指令，而它们
%% 其实是页面状态。
%%
%% `value` 按 schema 是字符串，但前端收的是任意可序列化值，路上谁 stringify 由
%% 客户端版本决定 —— 两种都认，非字符串按 JSON 打平。
-spec context_prompt(run_input() | [map()]) -> binary() | undefined.
context_prompt(Input) when is_map(Input) ->
    context_prompt(context(Input));
context_prompt(Items) when is_list(Items) ->
    case [L || I <- Items, (L = context_line(I)) =/= undefined] of
        [] -> undefined;
        Lines -> iolist_to_binary(
                   %% 必须写 /utf8：不写的话它是个码点列表，iolist_to_binary 直接 badarg
                   [<<"以下是前端在本轮提供的上下文（AG-UI context，非用户输入）：\n"/utf8>>,
                    lists:join(<<"\n">>, Lines)])
    end;
context_prompt(_) ->
    undefined.

%% @doc 共享 state → 一段 system 文本；空/非 map 不注入
%%
%% 这是 `inject_state` 的缺省渲染器。state 本来只给工具读写，模型看不到；
%% 有些应用需要它参与推理，但直接默认打开会把整份 UI state 塞进每次 LLM 请求，
%% token 成本和信息泄漏风险都应由宿主拍板。抬头明确它是共享状态、不是用户指令。
-spec state_prompt(term()) -> binary() | undefined.
state_prompt(State) when is_map(State), map_size(State) > 0 ->
    iolist_to_binary(
      [<<"以下是当前共享状态（AG-UI state，非用户输入）：\n"/utf8>>,
       beamai_utils:encode_json(State)]);
state_prompt(_) ->
    undefined.

%% @doc AG-UI 消息的 `content` → 纯文本
%%
%% 客户端可能发一个 `InputContent` 数组（多模态）。不翻的话下游拿到的是一坨
%% 结构体，内容全丢且不报错 —— 这里把其中的文本部件拼起来，非文本部件跳过
%% （骨架阶段不往下传图片，翻不动就别假装翻得动）。
-spec content_to_text(term()) -> binary().
content_to_text(C) when is_binary(C) ->
    C;
content_to_text(Parts) when is_list(Parts) ->
    iolist_to_binary([P || Part <- Parts, (P = part_text(Part)) =/= undefined]);
content_to_text(_) ->
    <<>>.

%%====================================================================
%% 内部
%%====================================================================

%% @private `resume[]`：interrupt 协议的答复数组
%%
%% 优先取 `interruptId` 与当前 pending 调用对得上的那条 —— 客户端可能一次带回
%% 多条（多路暂停），也可能带着一条**过期的**（上一轮的审批点了两次）。对不上就
%% 不认：拿别的 interrupt 的答复去恢复这一个，是把人的意思安到别处。
%%
%% 只有一条且不带 id 时宽容采纳：不少客户端只发 `[{status: "approved"}]`。
protocol_resume(Input, PendingToolCallId) ->
    case [E || E <- resume(Input), is_map(E)] of
        [] ->
            none;
        Entries ->
            case matching_resume(Entries, PendingToolCallId) of
                undefined -> none;
                Entry -> resume_entry(Entry)
            end
    end.

matching_resume(Entries, PendingToolCallId) ->
    case [E || E <- Entries, resume_id(E) =:= PendingToolCallId,
               PendingToolCallId =/= undefined] of
        [Match | _] ->
            Match;
        [] ->
            case [E || E <- Entries, resume_id(E) =:= undefined] of
                [Only] -> Only;
                _ -> undefined
            end
    end.

resume_id(Entry) ->
    case first_of(Entry, [<<"interruptId">>, <<"id">>]) of
        Id when is_binary(Id), Id =/= <<>> -> Id;
        _ -> undefined
    end.

%% @private 一条 `resume[]` → `{ok, Decision, Payload}`
%%
%% == `status` 不是 `decision` ==
%%
%% 协议里 `status` 说的是「这条 interrupt 答完了」（`resolved` / `completed`），
%% **不是**人做了什么决定。人的决定在 `payload.decision` 里 —— 那正是我们自己在
%% `responseSchema` 里要求的字段。曾经这里图省事按
%% `status > decision` 取第一个非空，于是客户端发
%% `{status:"resolved", payload:{decision:"rejected"}}` 时，我们读到的是
%% `"resolved"`，一路当成「自由文本作结果」发给模型 —— **批准与拒绝在这台上
%% 变成了同义词，而且一个字都不报**。
%%
%% 所以取值顺序是：`payload.decision` → 条目自己的 `decision` → `status`
%% **但只在它字面上就是一个决策词时**（有些客户端确实发
%% `[{status:"approved"}]`）。`"resolved"` 这种协议层的措辞一律不当决策。
%%
%% `payload.result` 是**前端工具**那条路的答复（这次调用返回了什么），它一出现
%% 就意味着 `reply` —— 载荷即工具结果，见 beamai_agui_runner 的收尾。
resume_entry(Entry) ->
    Payload = resume_payload(first_of(Entry, [<<"payload">>, <<"value">>])),
    case resume_decision(Entry, Payload) of
        undefined -> none;
        Decision -> {ok, Decision, Payload}
    end.

resume_decision(Entry, Payload) ->
    Explicit = first_of_decision(
                 [maps:get(decision, Payload, undefined),
                  maps:get(<<"decision">>, Entry, undefined),
                  maps:get(<<"outcome">>, Entry, undefined)]),
    case Explicit of
        undefined -> implicit_decision(Entry, Payload);
        Decision -> Decision
    end.

%% @private 没有显式决策时：有结果就是 reply，否则看 `status` 是不是决策词
implicit_decision(Entry, Payload) ->
    case maps:is_key(result, Payload) of
        true ->
            <<"reply">>;
        false ->
            case decision_word(maps:get(<<"status">>, Entry, undefined)) of
                undefined ->
                    %% `status: "resolved"` 且没有任何载荷 —— 客户端说"答完了"却
                    %% 什么都没给。**不编一个决策出来**：交给 runner 按 interrupt
                    %% 的类型决定怎么办（前端工具会如实报"没回传结果"）。
                    case maps:get(<<"status">>, Entry, undefined) of
                        S when is_binary(S), S =/= <<>> -> <<"reply">>;
                        _ -> undefined
                    end;
                Word ->
                    Word
            end
    end.

first_of_decision(Values) ->
    case [W || V <- Values, (W = decision_word(V)) =/= undefined] of
        [W | _] -> W;
        [] -> undefined
    end.

%% @private 客户端的措辞五花八门，归一到上游 beamai_agent_interrupt 认的三个关键词
%%
%% **认不出就返回 undefined**（而不是原样透传）：上游把不认识的 binary 当"自由文本
%% 作结果"，那对聊天式 HITL 是对的，但对一个协议字段就是把 `"resolved"` 当成了
%% 工具的返回值。要走自由文本那条路，得是**人真的打了一句话**（最后一条 user
%% 消息），不是某个字段恰好是字符串。
decision_word(D) when is_binary(D) ->
    case string:lowercase(D) of
        <<"approved">> -> <<"approved">>;
        <<"approve">> -> <<"approved">>;
        <<"accept">> -> <<"approved">>;
        <<"accepted">> -> <<"approved">>;
        <<"rejected">> -> <<"rejected">>;
        <<"reject">> -> <<"rejected">>;
        <<"denied">> -> <<"rejected">>;
        <<"deny">> -> <<"rejected">>;
        <<"cancel">> -> <<"rejected">>;
        <<"cancelled">> -> <<"rejected">>;
        <<"reply">> -> <<"reply">>;
        <<"respond">> -> <<"reply">>;
        _ -> undefined
    end;
decision_word(D) when is_atom(D), D =/= undefined ->
    decision_word(atom_to_binary(D, utf8));
decision_word(_) ->
    undefined.

first_of(Map, Keys) ->
    case [V || K <- Keys, (V = maps:get(K, Map, undefined)) =/= undefined] of
        [V | _] -> V;
        [] -> undefined
    end.

%% @private forwardedProps.resume
explicit_resume(Input) ->
    case maps:get(<<"resume">>, forwarded_props(Input), undefined) of
        Map when is_map(Map) ->
            case maps:get(<<"decision">>, Map, undefined) of
                undefined -> none;
                Decision -> {ok, Decision, resume_payload(maps:get(<<"payload">>, Map, #{}))}
            end;
        _ ->
            none
    end.

%% @private 上游的 payload 用 atom 键（message / args），线上是 binary 键，转一次
%%
%% 只认这两个键：其余原样丢弃，不做 binary_to_atom —— 请求体是外部输入，
%% 拿它造原子等于给人一个撑爆原子表的口子。
resume_payload(Text) when is_binary(Text) ->
    %% 客户端把答复直接放成一个字符串（`payload: "ok"`），当作 message
    #{message => Text};
resume_payload(Payload) when is_map(Payload) ->
    lists:foldl(fun({Wire, Key}, Acc) ->
        case maps:get(Wire, Payload, undefined) of
            undefined -> Acc;
            Value -> Acc#{Key => Value}
        end
    end, #{}, [{<<"message">>, message}, {<<"args">>, args},
               %% 前端工具那条路的答复：这次调用**返回了什么**
               {<<"result">>, result},
               %% 审批那条路的答复：人做了什么**决定**
               {<<"decision">>, decision}]);
resume_payload(_) ->
    #{}.

%% @private 前端回传的 tool 结果，或人直接回的一句话
implicit_resume(Input, PendingToolCallId) ->
    case tool_reply(Input, PendingToolCallId) of
        {ok, Content} ->
            {ok, <<"reply">>, #{message => Content}};
        none ->
            case last_user_message(Input) of
                {ok, Text} when Text =/= <<>> -> {ok, Text, #{}};
                _ -> none
            end
    end.

%% @private 最后一条对得上 id 的 tool 消息
tool_reply(_Input, undefined) ->
    none;
tool_reply(Input, ToolCallId) ->
    Matching = [M || M <- messages(Input), is_tool_reply(M, ToolCallId)],
    case lists:reverse(Matching) of
        [Last | _] -> {ok, content_of(Last)};
        [] -> none
    end.

is_tool_reply(#{<<"role">> := <<"tool">>, <<"toolCallId">> := Id}, Id) -> true;
is_tool_reply(_, _) -> false.

is_user(#{<<"role">> := <<"user">>}) -> true;
is_user(_) -> false.

content_of(#{<<"content">> := C}) -> content_to_text(C);
content_of(_) -> <<>>.

%% @private context 一条 → 一行；空值跳过
context_line(Item) when is_map(Item) ->
    case context_value(maps:get(<<"value">>, Item, undefined)) of
        undefined ->
            undefined;
        Value ->
            case maps:get(<<"description">>, Item, undefined) of
                D when is_binary(D), D =/= <<>> -> <<"- ", D/binary, " ", Value/binary>>;
                _ -> <<"- ", Value/binary>>
            end
    end;
context_line(_) ->
    undefined.

context_value(undefined) -> undefined;
context_value(<<>>) -> undefined;
context_value(V) when is_binary(V) -> V;
context_value(V) -> beamai_utils:encode_json(V).

%% @private 一个 content 部件里的文本（认不出的形状跳过）
part_text(P) when is_binary(P) -> P;
part_text(#{<<"type">> := <<"text">>, <<"text">> := T}) when is_binary(T) -> T;
part_text(#{<<"text">> := T}) when is_binary(T) -> T;
part_text(_) -> undefined.

get_id(Map, Key, Prefix) ->
    case maps:get(Key, Map, undefined) of
        Id when is_binary(Id), Id =/= <<>> -> Id;
        _ -> beamai_id:gen_id(Prefix)
    end.

get_list(Map, Key) ->
    case maps:get(Key, Map, []) of
        L when is_list(L) -> L;
        _ -> []
    end.

get_map(Map, Key) ->
    case maps:get(Key, Map, #{}) of
        M when is_map(M) -> M;
        _ -> #{}
    end.

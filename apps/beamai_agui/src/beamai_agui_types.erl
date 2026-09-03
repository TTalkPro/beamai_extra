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
-export([thread_id/1, run_id/1, messages/1, tools/1, state/1, forwarded_props/1]).
-export([agent_name/1, last_user_message/1, resume_input/2]).

-export_type([run_input/0]).

-type run_input() :: #{
    thread_id := binary(),
    run_id := binary(),
    parent_run_id := binary() | undefined,
    messages := [map()],
    tools := [map()],
    context := [map()],
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

-spec messages(run_input()) -> [map()].
messages(#{messages := V}) -> V.

-spec tools(run_input()) -> [map()].
tools(#{tools := V}) -> V.

-spec state(run_input()) -> term().
state(#{state := V}) -> V.

-spec forwarded_props(run_input()) -> map().
forwarded_props(#{forwarded_props := V}) -> V.

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
%% 三条来路，按优先级：
%%
%% 1. `forwardedProps.resume` = `#{"decision" => D, "payload" => P}` —— 显式，
%%    能表达 approved / rejected / reply 全部语义，还能带 `args` 改参数。
%% 2. 最后一条 `role=tool` 且 `toolCallId` 对得上被中断调用的消息 —— 这是
%%    AG-UI 的**原生**路径：前端工具执行完，结果作为 tool 消息回传。
%%    映射成 `reply` 语义（工具不再由服务端执行，这条内容即结果）。
%% 3. 最后一条 user 消息 —— 聊天式 HITL：agent 停下来问，人直接回一句话。
%%    走上游的「自由文本作结果」回落。
%%
%% 三条都没有则 none —— 一个 thread 卡在暂停上时，不带任何输入的请求没法推进。
-spec resume_input(run_input(), binary() | undefined) ->
    {ok, term(), map()} | none.
resume_input(Input, PendingToolCallId) ->
    case explicit_resume(Input) of
        {ok, _D, _P} = Explicit -> Explicit;
        none -> implicit_resume(Input, PendingToolCallId)
    end.

%%====================================================================
%% 内部
%%====================================================================

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
resume_payload(Payload) when is_map(Payload) ->
    lists:foldl(fun({Wire, Key}, Acc) ->
        case maps:get(Wire, Payload, undefined) of
            undefined -> Acc;
            Value -> Acc#{Key => Value}
        end
    end, #{}, [{<<"message">>, message}, {<<"args">>, args}]);
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

content_of(#{<<"content">> := C}) when is_binary(C) -> C;
content_of(_) -> <<>>.

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

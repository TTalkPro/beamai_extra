%%%-------------------------------------------------------------------
%%% @doc 事件流插件的公共零件
%%%
%%% activity 类插件（`beamai_agui_a2ui` / `beamai_agui_mcp_apps`，以及 examples 里的
%%% `example_agui_genui`）干的是同一件事：**盯住某个工具的调用，把它的参数翻成
%%% activity 事件**。差别只在翻成什么形状。公共的那一半在这里 —— 所以宿主自己写
%%% 一个 activity 插件时，要抄的只是 `on_args` 那个函数。
%%%
%%% == 为什么状态放进程字典 ==
%%%
%%% transform 的签名是 `fun((Event) -> [Event])` —— 一个函数，没有地方串状态，
%%% 而攒参数片段这件事必须有状态。Erlang 里三条路：ETS（要管生命周期）、
%%% 给签名加个状态参数（污染所有插件）、进程字典。
%%%
%%% 选进程字典是因为**一次 run 就是一个进程**（`beamai_agui_session` 给每个 run
%%% spawn 一个，子 agent lane 是 manager 的 worker 进程），所以「按 run 隔离」
%%% 这件事**由进程边界天然给到**，不需要自己造一层。key 里带一个构造期的
%%% `make_ref()`，同一个进程里挂两个插件也互不干扰。
%%%
%%% == 为什么在 TOOL_CALL_END 才翻译 ==
%%%
%%% `TOOL_CALL_ARGS` 的 delta 是 **JSON 文本片段**（`beamai_agui_raw` 逐字流出来
%%% 的那些），单独一片解析不了。攒到 `TOOL_CALL_END` 再一次解析，对流式与非流式
%%% 两条路都成立 —— 非流式那条本来就是一片装完整的 JSON。
%%%
%%% == 中间态（可选）==
%%%
%%% 「边生成边长出来」的原料是 `TOOL_CALL_ARGS` 的**半截 JSON**：给它配一个
%%% `on_partial`（与 `on_args` 同形，收的是「已经写完的那一段」），插件就会在流式
%%% 过程中反复发中间态 —— A2UI 靠 `replace: true` + 稳定的 messageId，前端整块
%%% 换掉，组件树就一点点长出来。扫描在 `beamai_agui_json_prefix`（找切点，不写
%%% 完整的增量 parser）。节流与去重在公共层：每 `partial_every` 字节（缺省 200）
%%% 扫一次，内容没变就不发。**不配 `on_partial` 就一条中间态都没有**，行为与
%%% 之前完全一致。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_plugin).

-export([tool_watcher/1, compose/1]).
-export([add_tool/2, append_system_prompt/2]).

-type event() :: beamai_agui_event:event().
-type transform() :: fun((event()) -> [event()]).

%% - `tools`      盯哪几个工具名
%% - `on_args`    `fun((ToolCallId, ToolName, Args) -> [event()])`，参数到齐时调用
%% - `on_partial` `fun((ToolCallId, ToolName, ArgsPrefix) -> [event()])`，流式过程中的
%%                中间态（不配就没有）；`ArgsPrefix` 是半截 JSON 里已经写完的那段
%% - `partial_every` 中间态的扫描节流（字节，缺省 200）
%% - `hold_start` 扣住 `TOOL_CALL_START` / `ARGS` 直到 activity 先发出去（见下）
-type watcher_opts() :: #{
    tools := [binary()],
    on_args := fun((binary(), binary(), map()) -> [event()]),
    on_partial => fun((binary(), binary(), map()) -> [event()]),
    partial_every => pos_integer(),
    hold_start => boolean()
}.

-export_type([transform/0, watcher_opts/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 造一个「盯住某几个工具、参数到齐时产出 activity 事件」的 transform
%%
%% `hold_start` 为真时，被盯工具的 `TOOL_CALL_START` / `TOOL_CALL_ARGS` **扣住不发**，
%% 等到参数到齐，先放第一条 activity，再放扣住的那些。为的是别让前端先看见一张
%% 工具卡片、再看见 UI 长出来 —— 闪一下。A2UI 不扣（上游也不扣，工具卡片先出、面
%% 随后贴上是它接受的顺序），生成式 UI 扣。
%%
%% **扣住的事件一定要在 `TOOL_CALL_END` 放完**：终态事件不过 transform，扣着不放
%% 就是真丢了。
-spec tool_watcher(watcher_opts()) -> transform().
tool_watcher(#{tools := _Names, on_args := _OnArgs} = Opts) ->
    Key = {?MODULE, make_ref()},
    fun(Event) -> watch(Key, Opts, Event) end.

%% @doc 把若干 transform 串成一个（前一个的产出喂给后一个）
%%
%% 装两个以上插件时用。顺序即层序：`compose([A, B])` 里 A 先看到原始事件，
%% B 看到的是 A 产出的那些。
-spec compose([transform()]) -> transform().
compose([]) ->
    fun(Event) -> [Event] end;
compose([Single]) ->
    Single;
compose(Transforms) ->
    fun(Event) ->
        lists:foldl(fun(T, Events) -> lists:flatmap(fun(E) -> apply_one(T, E) end, Events) end,
                    [Event], Transforms)
    end.

%% @doc 往一份 agent 配置里追加一个内联工具
%%
%% **工具挂在 chat client 上，不是 agent 配置的某个键** —— `beamai_agent:new/1`
%% 根本没有 `tools` 这一位（只有 `interrupt_tools`）。写成 `Config#{tools => ...}`
%% 编译得过、跑得动、模型却**永远看不见那个工具** —— 实机联调时才照出来：模型
%% 反复去调别的工具，还振振有词地说"没有可用的 render_a2ui"。
%%
%% 所以配置里没有 `chat_client` 就**直接报错**，不静默放过：一个装不上的插件
%% 应当在装配期炸，而不是等到线上发现模型行为不对。
-spec add_tool(map(), map()) -> map().
add_tool(#{chat_client := Client} = Config, Tool) ->
    Config#{chat_client => beamai_chat_client:add_tool(Client, Tool)};
add_tool(_Config, _Tool) ->
    error({beamai_agui_plugin, no_chat_client,
           <<"with_tool/1,2 要在配置里已经有 chat_client 之后调用"/utf8>>}).

%% @doc 往一份 agent 配置的 system prompt 后面追加一段
%%
%% **追加不覆盖**：插件的用法说明是加在宿主人设后面的，不是替它。
-spec append_system_prompt(map(), binary()) -> map().
append_system_prompt(Config, Text) ->
    case maps:get(system_prompt, Config, undefined) of
        P when is_binary(P), P =/= <<>> ->
            Config#{system_prompt => <<P/binary, "\n\n", Text/binary>>};
        _ ->
            Config#{system_prompt => Text}
    end.

%%====================================================================
%% 内部 - watcher
%%====================================================================

watch(Key, Opts, Event) ->
    case beamai_agui_event:type_of(Event) of
        <<"TOOL_CALL_START">> -> on_start(Key, Opts, Event);
        <<"TOOL_CALL_ARGS">> -> on_args(Key, Opts, Event);
        <<"TOOL_CALL_END">> -> on_end(Key, Opts, Event);
        _ -> [Event]
    end.

on_start(Key, Opts, Event) ->
    Names = maps:get(tools, Opts),
    Hold = maps:get(hold_start, Opts, false),
    Id = maps:get(<<"toolCallId">>, Event, undefined),
    Name = maps:get(<<"toolCallName">>, Event, undefined),
    case lists:member(Name, Names) of
        false ->
            [Event];
        true ->
            Held = case Hold of true -> [Event]; false -> [] end,
            put_call(Key, Id, #{name => Name, buf => [], held => Held}),
            case Hold of true -> []; false -> [Event] end
    end.

on_args(Key, Opts, Event) ->
    Id = maps:get(<<"toolCallId">>, Event, undefined),
    case get_call(Key, Id) of
        undefined ->
            [Event];
        #{buf := Buf, held := Held, name := Name} = Call ->
            Hold = maps:get(hold_start, Opts, false),
            Delta = maps:get(<<"delta">>, Event, <<>>),
            Held1 = case Hold of true -> Held ++ [Event]; false -> Held end,
            Buf1 = Buf ++ [Delta],
            Call1 = Call#{buf => Buf1, held => Held1},
            {Partials, Call2} = partial_events(Opts, Id, Name, Buf1, Call1),
            put_call(Key, Id, Call2),
            case Hold of
                true -> Partials;
                false -> [Event | Partials]
            end
    end.

on_end(Key, Opts, Event) ->
    OnArgs = maps:get(on_args, Opts),
    Hold = maps:get(hold_start, Opts, false),
    Id = maps:get(<<"toolCallId">>, Event, undefined),
    case take_call(Key, Id) of
        undefined ->
            [Event];
        #{name := Name, buf := Buf, held := Held} ->
            Activities = safe_on_args(OnArgs, Id, Name, decode_args(Buf)),
            case {Hold, Activities} of
                {false, _} ->
                    [Event | Activities];
                {true, []} ->
                    %% 一条 activity 都没产出（参数是空的 / 解析不出来）：
                    %% 扣住的照样得放，否则前端连工具调用都看不见
                    Held ++ [Event];
                {true, [First | Rest]} ->
                    [First] ++ Held ++ Rest ++ [Event]
            end
    end.

%% @private 插件炸了不该把 run 带走 —— 它是旁路增强
safe_on_args(OnArgs, Id, Name, Args) ->
    try OnArgs(Id, Name, Args) of
        Events when is_list(Events) -> Events;
        _ -> []
    catch
        Class:Reason:Stack ->
            logger:warning("beamai_agui plugin on_args crashed: ~p:~p", [Class, Reason],
                           #{stacktrace => Stack}),
            []
    end.

%% @private 流式中间态：攒够 `partial_every` 字节扫一次，内容没变不发
%%
%% **没配 `on_partial` 就什么都不做** —— 这条路是 2026-10 新加的，不该凭空改变
%% 既有插件的事件序列。
partial_events(Opts, Id, Name, Buf, Call) ->
    OnPartial = maps:get(on_partial, Opts, undefined),
    Scanned = maps:get(scanned, Call, 0),
    case is_function(OnPartial, 3)
         andalso iolist_size(Buf) - Scanned >= partial_every(Opts) of
        false ->
            {[], Call};
        true ->
            case beamai_agui_json_prefix:parse_prefix(iolist_to_binary(Buf)) of
                {ok, Prefix} when is_map(Prefix) ->
                    case Prefix =/= maps:get(prefix, Call, none) of
                        false ->
                            {[], Call#{scanned => iolist_size(Buf)}};
                        true ->
                            Events = safe_on_partial(OnPartial, Id, Name, Prefix),
                            {Events, Call#{scanned => iolist_size(Buf),
                                           prefix => Prefix}}
                    end;
                _Other ->
                    %% 无效前缀也要推进节流位，否则每个 delta 都重复扫坏输入
                    {[], Call#{scanned => iolist_size(Buf)}}
            end
    end.

%% @private 中间态的节流粒度（字节）
partial_every(Opts) ->
    maps:get(partial_every, Opts, 200).

safe_on_partial(OnPartial, Id, Name, Prefix) ->
    try OnPartial(Id, Name, Prefix) of
        Events when is_list(Events) -> Events;
        _ -> []
    catch
        Class:Reason:Stack ->
            logger:warning("beamai_agui plugin on_partial crashed: ~p:~p", [Class, Reason],
                           #{stacktrace => Stack}),
            []
    end.

%% @private 攒起来的片段 → 参数 map（解不出来就当空参数，不让整条流出事）
decode_args(Buf) ->
    case iolist_to_binary(Buf) of
        <<>> ->
            #{};
        Json ->
            %% 解不出来就当空参数 —— 半截 JSON（模型被截断）不该让整条流出事
            try beamai_utils:parse_json(Json)
            catch _:_ -> #{}
            end
    end.

%%====================================================================
%% 内部 - 每 run 的状态（见模块头）
%%====================================================================

put_call(Key, Id, Call) ->
    put(Key, maps:put(Id, Call, calls(Key))),
    ok.

get_call(Key, Id) ->
    maps:get(Id, calls(Key), undefined).

take_call(Key, Id) ->
    Calls = calls(Key),
    case maps:take(Id, Calls) of
        {Call, Rest} -> put(Key, Rest), Call;
        error -> undefined
    end.

calls(Key) ->
    case get(Key) of
        undefined -> #{};
        Calls -> Calls
    end.

apply_one(Transform, Event) ->
    case Transform(Event) of
        Events when is_list(Events) -> Events;
        Rewritten when is_map(Rewritten) -> [Rewritten];
        drop -> [];
        _ -> [Event]
    end.

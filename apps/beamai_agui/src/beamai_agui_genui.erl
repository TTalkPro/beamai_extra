%%%-------------------------------------------------------------------
%%% @doc Open Generative UI —— 模型直接生成一块**沙箱 UI**（可选插件）
%%%
%%% 移植自 CopilotKit 的 `open-generative-ui-middleware`：工具名
%%% `generateSandboxedUi`、活动类型 `open-generative-ui`、`ACTIVITY_SNAPSHOT` /
%%% `ACTIVITY_DELTA` 的 JSON Patch 形状全部对齐，前端那半边（沙箱 iframe 的
%%% renderer）原样可用。
%%%
%%% == 它是可选插件，不是新机制 ==
%%%
%%% 两个挂点，都不进主路径，不装就完全不存在：
%%%
%%% | 要什么 | 挂在哪 |
%%% |---|---|
%%% | 让模型看得见这个工具 | `with_tool/1,2`：往 agent 配置里塞一个内联工具 + 设计规范 |
%%% | 把工具调用翻成 activity 事件 | `event_transform/0,1` |
%%%
%%% 别忘了 `/info` 的 `openGenerativeUIEnabled`（路由配置里 `open_generative_ui => true`）
%%% —— 前端据此才会注册它那半边的 renderer。
%%%
%%% == 后台干的事 ==
%%%
%%% 模型调 `generateSandboxedUi`，参数是一大坨
%%% `{initialHeight, placeholderMessages, css, html, jsFunctions, jsExpressions}`。
%%% 后台**不执行**它 —— 把参数翻成一串 activity 事件推给前端：
%%%
%%% ```
%%% ACTIVITY_SNAPSHOT  {initialHeight, generating: true}   ← 必须最早，且只一条
%%% ACTIVITY_DELTA     [{op add path /css        value …}]
%%% ACTIVITY_DELTA     [{op add path /cssComplete value true}]
%%% ACTIVITY_DELTA     [{op add path /html       value []}]   ← 先建数组
%%% ACTIVITY_DELTA     [{op add path /html/-     value "<div…"}] ← 再追加
%%% …
%%% ACTIVITY_DELTA     [{op add path /generating value false}]  ← 收尾
%%% ```
%%%
%%% 三条**猜错了前端不报错、只会静默少渲染**的细节（都是上游踩出来的）：
%%%
%%% 1. **snapshot 必须在任何 delta 之前**，而且只发一条。前端把 delta 打在
%%%    `messageId` 对应的 activity 消息上，消息还没建就悄悄丢弃；
%%% 2. **`add` 操作必须带 `value`**。模型经常给 `"jsFunctions": null`，发一条没有
%%%    value 的 patch 会让前端的 JSON Patch 实现把**整批**判非法丢掉。所以值是
%%%    null 就整条跳过，只留 `…Complete` 标记；
%%% 3. **`html` 是数组不是字符串**：`/html` 先建空数组，之后每块 `/html/-` 追加。
%%%    前端把它 join 起来渲染，这样才有「UI 一点点长出来」的效果。
%%%
%%% == 与上游的一处真实差异 ==
%%%
%%% 上游吃的是**流式** tool-call 参数，用增量 JSON 扫描器一边解析一边贴。我们攒到
%%% `TOOL_CALL_END` 一次翻译（见 `beamai_agui_plugin` 模块头），所以 html 只有
%%% **一块**：事件序列、顺序、形状与上游逐条一致，少的是「边生成边长出来」。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_genui).

-export([tool_name/0, activity_type/0, design_skill/0]).
-export([sandboxed_ui_tool/0, sandboxed_ui_tool/1, with_tool/1, with_tool/2]).
-export([event_transform/0, event_transform/1]).
-export([activity_events/2]).

-define(TOOL_NAME, <<"generateSandboxedUi">>).
-define(ACTIVITY_TYPE, <<"open-generative-ui">>).

%%====================================================================
%% 常量
%%====================================================================

-spec tool_name() -> binary().
tool_name() -> ?TOOL_NAME.

-spec activity_type() -> binary().
activity_type() -> ?ACTIVITY_TYPE.

%%====================================================================
%% 工具与 agent 配置
%%====================================================================

%% @doc 沙箱 UI 工具
%%
%% **handler 不生成任何 UI** —— UI 就是参数本身，后台只负责把它翻成 activity
%% 事件；handler 只是给模型一个「干完了」的回执，好让它继续说话。上游把这个
%% handler 放在前端，我们放在服务端 —— 于是**不依赖前端注册工具**，任何 AG-UI
%% 客户端都能用。
-spec sandboxed_ui_tool() -> map().
sandboxed_ui_tool() -> sandboxed_ui_tool(?TOOL_NAME).

-spec sandboxed_ui_tool(binary()) -> map().
sandboxed_ui_tool(Name) ->
    #{name => Name,
      description => description(),
      parameters => #{
          <<"initialHeight">> => #{type => number,
                                   description => <<"Initial iframe height in pixels">>},
          <<"placeholderMessages">> => #{type => array, items => #{type => string},
                                         description => <<"Messages shown while generating">>},
          <<"css">> => #{type => string, description => <<"All styles, generated first">>},
          <<"html">> => #{type => string, description => <<"The UI markup">>},
          <<"jsFunctions">> => #{type => string,
                                 description => <<"Reusable helper functions">>},
          <<"jsExpressions">> => #{type => array, items => #{type => string},
                                   description => <<"Expressions applied one by one">>}
      },
      handler => fun(_Args) -> {ok, <<"UI generated">>} end}.

%% @doc 把工具与设计规范挂进一份 agent 配置
%%
%% `design_skill => undefined` 就不追加提示词（你自己在 system prompt 里写）。
-spec with_tool(map()) -> map().
with_tool(Config) -> with_tool(Config, #{}).

-spec with_tool(map(), map()) -> map().
with_tool(Config, Opts) ->
    Name = maps:get(tool_name, Opts, ?TOOL_NAME),
    WithTool = beamai_agui_plugin:add_tool(Config, sandboxed_ui_tool(Name)),
    case maps:get(design_skill, Opts, design_skill()) of
        undefined -> WithTool;
        <<>> -> WithTool;
        Skill -> beamai_agui_plugin:append_system_prompt(WithTool, Skill)
    end.

%%====================================================================
%% 参数 → activity 事件
%%====================================================================

%% @doc 一次调用的参数 → 完整的 activity 事件序列
%%
%% 键的处理顺序是**约定的那一个**（`initialHeight` → `placeholderMessages` → `css`
%% → `html` → `jsFunctions` → `jsExpressions`）：模型的原始 key 顺序在 provider 解析
%% JSON 时就没了，而这个特性的提示词恰恰按顺序要求生成 —— 按约定重排，比听天由命
%% 诚实。
-spec activity_events(binary(), map()) -> [beamai_agui_event:event()].
activity_events(ToolCallId, Args) ->
    MessageId = <<ToolCallId/binary, "-activity">>,
    Height = maps:get(<<"initialHeight">>, Args, undefined),
    Snapshot = beamai_agui_event:activity_snapshot(
                 MessageId, ?ACTIVITY_TYPE,
                 case is_number(Height) of
                     true -> #{<<"generating">> => true, <<"initialHeight">> => Height};
                     false -> #{<<"generating">> => true}
                 end),
    Deltas =
        array_of(MessageId, <<"placeholderMessages">>, Args)
        ++ scalar_of(MessageId, <<"css">>, Args)
        ++ html_of(MessageId, Args)
        ++ scalar_of(MessageId, <<"jsFunctions">>, Args)
        ++ array_of(MessageId, <<"jsExpressions">>, Args)
        ++ [delta(MessageId, <<"/generating">>, false)],
    [Snapshot | Deltas].

%% @doc `event_transform`：`generateSandboxedUi` 的调用 → activity 事件
%%
%% **扣住 `TOOL_CALL_START`** 直到第一条 activity 发出去 —— 否则前端先看见一张
%% 工具卡片，再看见 UI，闪一下。
-spec event_transform() -> beamai_agui_plugin:transform().
event_transform() -> event_transform(#{}).

-spec event_transform(map()) -> beamai_agui_plugin:transform().
event_transform(Opts) ->
    Name = maps:get(tool_name, Opts, ?TOOL_NAME),
    beamai_agui_plugin:tool_watcher(
      #{tools => [Name],
        hold_start => true,
        on_args => fun(ToolCallId, _N, Args) -> activity_events(ToolCallId, Args) end}).

%%====================================================================
%% 内部 - patch 构造
%%====================================================================

delta(MessageId, Path, Value) ->
    beamai_agui_event:activity_delta(
      MessageId, ?ACTIVITY_TYPE,
      [#{<<"op">> => <<"add">>, <<"path">> => Path, <<"value">> => Value}]).

%% @private 标量键：值 + 完成标记。**null 整条跳过**（见模块头第 2 条）
scalar_of(MessageId, Key, Args) ->
    case maps:get(Key, Args, undefined) of
        undefined -> [];
        null -> [];
        Value -> [delta(MessageId, <<"/", Key/binary>>, Value),
                  delta(MessageId, <<"/", Key/binary, "Complete">>, true)]
    end.

%% @private 数组键：先建空数组，再逐项追加（`jsExpressions` 另有完成标记）
array_of(MessageId, Key, Args) ->
    case maps:get(Key, Args, undefined) of
        Items when is_list(Items), Items =/= [] ->
            [delta(MessageId, <<"/", Key/binary>>, [])]
            ++ [delta(MessageId, <<"/", Key/binary, "/-">>, I) || I <- Items]
            ++ complete_marker(MessageId, Key);
        _ ->
            []
    end.

complete_marker(MessageId, <<"jsExpressions">>) ->
    [delta(MessageId, <<"/jsExpressionsComplete">>, true)];
complete_marker(_MessageId, _Key) ->
    [].

%% @private html 是**数组**不是字符串（见模块头第 3 条）
html_of(MessageId, Args) ->
    case maps:get(<<"html">>, Args, undefined) of
        Html when is_binary(Html), Html =/= <<>> ->
            [delta(MessageId, <<"/html">>, []),
             delta(MessageId, <<"/html/-">>, Html),
             delta(MessageId, <<"/htmlComplete">>, true)];
        _ ->
            []
    end.

%%====================================================================
%% 内部 - 提示词
%%====================================================================

%% @private 工具描述 —— **这是提示词，不是文档**：沙箱的限制、可用的逃生口、
%% 以及参数顺序都靠它传达给模型。
description() ->
    <<"Generate sandboxed UI. "
      "IMPORTANT: The generated code runs in a sandboxed iframe WITHOUT same-origin access. "
      "Do NOT use localStorage, sessionStorage, document.cookie, IndexedDB, or fetch to "
      "same-origin URLs. To communicate with the host application, use "
      "Websandbox.connection.remote.<functionName>(args) which returns a Promise.\n\n"
      "You CAN use external libraries from CDNs by including <script> or <link> tags in the "
      "HTML <head> (e.g. Chart.js, D3, Three.js). CDN resources load normally inside the sandbox.\n\n"
      "PARAMETER ORDER IS CRITICAL — generate parameters in exactly this order:\n"
      "1. initialHeight + placeholderMessages (shown to user while generating)\n"
      "2. css (all styles FIRST — the user sees a placeholder until CSS is complete)\n"
      "3. html (the UI markup)\n"
      "4. jsFunctions (reusable helper functions)\n"
      "5. jsExpressions (applied one-by-one)">>.

%% @doc 设计规范 —— 上游把它作为 agent context 每轮带给模型，这里作为 system prompt
%% 的追加段：语义相同，少一个机制。
-spec design_skill() -> binary().
design_skill() ->
    <<"When generating UI with generateSandboxedUi, follow these design principles "
      "inspired by shadcn/ui:\n\n"
      "- Use a minimal, flat aesthetic. Avoid drop shadows and gradients — rely on subtle "
      "borders (1px solid, light gray like #e5e7eb) to define surfaces.\n"
      "- Neutral base palette: white backgrounds, zinc/slate gray text (#09090b for headings, "
      "#71717a for secondary text). One accent color for interactive elements.\n"
      "- Use system font stacks (system-ui, -apple-system, sans-serif) at readable sizes "
      "(14px body, 600 weight for headings). Tight line-heights.\n"
      "- Small, consistent border-radius (6-8px). Cards and containers use border, not shadow.\n"
      "- Buttons: solid fill for primary (dark bg, white text), outline for secondary. "
      "Subtle hover state.\n"
      "- Use CSS Grid or Flexbox for layout. Ensure the UI looks good at any width.\n"
      "- Minimal transitions (150ms) for hover/focus states only. No decorative animations.\n"
      "- Keep the UI focused and dense — compact spacing (8-12px gaps, 10-14px padding).">>.

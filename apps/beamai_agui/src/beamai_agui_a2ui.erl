%%%-------------------------------------------------------------------
%%% @doc A2UI —— 声明式生成式 UI（可选插件）
%%%
%%% 移植自 `@ag-ui/a2ui-middleware`。与 Open Generative UI
%%% （`example_agui_genui`，在 examples 里）的分工：
%%%
%%% | | `example_agui_genui`（Open Generative UI） | 本模块（A2UI） |
%%% |---|---|---|
%%% | 模型生成什么 | 一整块 HTML/CSS/JS | **组件树**（只能用 catalog 里有的组件） |
%%% | 谁来渲染 | 沙箱 iframe 执行模型写的代码 | 前端按自己的组件库渲染 |
%%% | 可控性 | 任意 JS（所以要沙箱） | **白名单**：模型编不出 catalog 之外的组件 |
%%% | 交互回流 | 沙箱自己的通道 | `forwardedProps.a2uiAction`（见 input_transform/0） |
%%%
%%% 要「像素级自由」用 genui，要「设计系统一致 + 不执行任意代码」用 A2UI。
%%%
%%% **A2UI 留在库里、genui 在 examples 里**，判据是「协议认不认」：A2UI 有自己的
%%% 外部规范（catalog id 指向 `a2ui.org/specification/v0_9/...`），不绑某一个前端
%%% runtime；而 genui 那套工具名与 patch 形状是 CopilotKit Runtime 一个可选中间件
%%% 自己的约定 —— 见 `example_agui_genui` 模块头。
%%%
%%% == 装 ==
%%%
%%% ```erlang
%%% %% 工厂里：
%%% beamai_agent:new(maps:merge(beamai_agui_a2ui:with_tool(#{chat_client => K}), RuntimeCfg))
%%%
%%% %% 路由里：
%%% #{agent => <<"my-agent">>,
%%%   event_transform => beamai_agui_a2ui:event_transform(),
%%%   input_transform => beamai_agui_a2ui:input_transform(),
%%%   a2ui => true}                         %% /info 的能力位
%%% ```
%%%
%%% == 后台干什么 ==
%%%
%%% 1. **注入工具与用法**（`with_tool/1,2`）：`render_a2ui` 工具 + 一段用法提示词 +
%%%    **catalog**（有哪些组件、各自什么属性）。catalog 是这条路的关键 —— 它既是给
%%%    模型的词汇表，也是「模型编不出别的组件」的保证；
%%% 2. **翻译事件**（`event_transform/0,1`）：模型调 `render_a2ui`，参数
%%%    `{surfaceId, components, data}` 翻成一条 `ACTIVITY_SNAPSHOT`，`content` 里装
%%%    三条 op：`createSurface` → `updateComponents` → `updateDataModel`。
%%%    **顺序是协议要求的**（先建面，再给组件，最后灌数据），少一条或反了，前端
%%%    要么空白要么报「surface 不存在」。`replace: true` 表示整块换掉，所以不需要
%%%    delta —— 每次都是完整的一份；
%%% 3. **回流用户动作**（`input_transform/0`）：用户点了生成出来的按钮，前端把
%%%    `forwardedProps.a2uiAction` 发上来，翻成一句话喂回模型。
%%%
%%% == 与上游的两处差异 ==
%%%
%%% 1. 上游从流式 tool-call 参数里增量抠出「已经完整的那几个组件」，一边生成一边
%%%    往面上贴。我们攒到 `TOOL_CALL_END` 一次翻译（见 `beamai_agui_plugin` 模块头），
%%%    **一条快照发完** —— 形状与顺序一致，少的是中间态；
%%% 2. 上游把用户动作合成成一次工具调用 + 工具结果塞进历史。我们的历史是服务端
%%%    权威（客户端塞不进消息），所以翻成一条**用户消息** —— 语义相同，路径不同。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_a2ui).

-export([tool_name/0, activity_type/0, catalog_id/0, basic_catalog/0]).
-export([render_tool/0, render_tool/1, with_tool/1, with_tool/2]).
-export([event_transform/0, event_transform/1]).
-export([input_transform/0, user_action_message/1]).
-export([surface_ops/2, surface_event/3]).

-define(TOOL_NAME, <<"render_a2ui">>).
-define(ACTIVITY_TYPE, <<"a2ui-surface">>).
%% `content` 里装 op 数组的键。**字符串键** —— 它要原样出现在 JSON 里。
-define(OPERATIONS_KEY, <<"a2ui_operations">>).
-define(CATALOG_ID,
        <<"https://a2ui.org/specification/v0_9/catalogs/basic/catalog.json">>).
-define(VERSION, <<"v0.9">>).

%%====================================================================
%% 常量
%%====================================================================

-spec tool_name() -> binary().
tool_name() -> ?TOOL_NAME.

%% @doc activity 消息的类型标签。前端按它挑 renderer。
-spec activity_type() -> binary().
activity_type() -> ?ACTIVITY_TYPE.

%% @doc A2UI v0.9 基础 catalog 的 id
-spec catalog_id() -> binary().
catalog_id() -> ?CATALOG_ID.

%%====================================================================
%% 工具与 agent 配置
%%====================================================================

%% @doc `render_a2ui` 的工具定义
%%
%% **handler 不画任何东西** —— UI 就是参数本身，后台只负责把它翻成 activity 事件；
%% handler 只给模型一个「贴上去了」的回执，好让它继续说话。
-spec render_tool() -> map().
render_tool() -> render_tool(?TOOL_NAME).

-spec render_tool(binary()) -> map().
render_tool(Name) ->
    #{name => Name,
      description => <<"Render a dynamic A2UI v0.9 surface with structured parameters. "
                       "Follow the A2UI render tool usage guide provided in context.">>,
      parameters => #{
          <<"surfaceId">> => #{type => string, required => true,
                               description => <<"Unique surface identifier.">>},
          <<"components">> => #{type => array, required => true,
                                items => #{type => object},
                                description =>
                                    <<"A2UI v0.9 component array (flat format). "
                                      "The root component must have id \"root\".">>},
          <<"data">> => #{type => object,
                          description =>
                              <<"Initial data model for the surface. Written to the root "
                                "path. Use for pre-filling form values or providing data "
                                "for components bound to data model paths.">>}
      },
      handler => fun(Args) ->
          {ok, <<"Surface \"", (surface_id(Args))/binary, "\" rendered.">>}
      end}.

%% @doc 把工具、用法提示词、catalog 挂进一份 agent 配置
%%
%% - `catalog`   `#{<<"catalogId">> => ..., <<"components">> => #{名字 => Schema}}`，
%%               缺省 `basic_catalog/0`。**传你前端真正注册的那份** —— catalog 是
%%               模型的词汇表，报了前端没有的组件，渲染就是空白；
%% - `tool_name` 自定义工具名（缺省 `render_a2ui`）。
-spec with_tool(map()) -> map().
with_tool(Config) -> with_tool(Config, #{}).

-spec with_tool(map(), map()) -> map().
with_tool(Config, Opts) ->
    Name = maps:get(tool_name, Opts, ?TOOL_NAME),
    Catalog = maps:get(catalog, Opts, basic_catalog()),
    Context = <<(guidelines(Name))/binary,
                "\n\n### Available Components -- the A2UI catalog you may use. "
                "Only these component names exist.\n\n```json\n",
                (beamai_utils:encode_json(Catalog))/binary, "\n```">>,
    beamai_agui_plugin:append_system_prompt(
      beamai_agui_plugin:add_tool(Config, render_tool(Name)), Context).

%%====================================================================
%% 参数 → activity 事件
%%====================================================================

%% @doc `render_a2ui` 的参数 → A2UI v0.9 op 数组
%%
%% **顺序是协议要求的**：先 `createSurface` 建面，再 `updateComponents` 给组件，
%% 最后（有 data 才发）`updateDataModel` 灌数据。
-spec surface_ops(map(), binary()) -> [map()].
surface_ops(Args, CatalogId) ->
    Sid = surface_id(Args),
    Components = case maps:get(<<"components">>, Args, []) of
                     L when is_list(L) -> L;
                     _ -> []
                 end,
    Base = [#{<<"version">> => ?VERSION,
              <<"createSurface">> => #{<<"surfaceId">> => Sid,
                                       <<"catalogId">> => CatalogId}},
            #{<<"version">> => ?VERSION,
              <<"updateComponents">> => #{<<"surfaceId">> => Sid,
                                          <<"components">> => Components}}],
    case maps:get(<<"data">>, Args, undefined) of
        Data when is_map(Data), map_size(Data) > 0 ->
            Base ++ [#{<<"version">> => ?VERSION,
                       <<"updateDataModel">> => #{<<"surfaceId">> => Sid,
                                                  <<"path">> => <<"/">>,
                                                  <<"value">> => Data}}];
        _ ->
            Base
    end.

%% @doc 一条 `ACTIVITY_SNAPSHOT`（`replace` = 整块换掉，所以不需要 delta）
-spec surface_event(binary(), map(), binary()) -> beamai_agui_event:event().
surface_event(ToolCallId, Args, CatalogId) ->
    beamai_agui_event:activity_snapshot(
      <<?ACTIVITY_TYPE/binary, "-", ToolCallId/binary>>,
      ?ACTIVITY_TYPE,
      #{?OPERATIONS_KEY => surface_ops(Args, CatalogId)},
      true).

%% @doc `event_transform`：`render_a2ui` 的调用 → surface 快照
%%
%% **不扣住 `TOOL_CALL_START`**：上游 A2UI 也不扣 —— 工具卡片先出、面随后贴上，
%% 是它接受的顺序（genui 那边扣是因为沙箱 UI 一闪的观感，两边取舍不同）。
-spec event_transform() -> beamai_agui_plugin:transform().
event_transform() -> event_transform(#{}).

-spec event_transform(map()) -> beamai_agui_plugin:transform().
event_transform(Opts) ->
    Name = maps:get(tool_name, Opts, ?TOOL_NAME),
    CatalogId = maps:get(catalog_id, Opts, ?CATALOG_ID),
    beamai_agui_plugin:tool_watcher(
      #{tools => [Name],
        on_args => fun(ToolCallId, _N, Args) -> [surface_event(ToolCallId, Args, CatalogId)] end}).

%%====================================================================
%% 入站：用户在生成出来的界面上点了什么
%%====================================================================

%% @doc `forwardedProps.a2uiAction.userAction` → 一句喂回模型的话
-spec user_action_message(map()) -> binary().
user_action_message(Action) ->
    Name = get_str(Action, <<"name">>, <<"unknown_action">>),
    Surface = get_str(Action, <<"surfaceId">>, <<"unknown_surface">>),
    Component = case maps:get(<<"sourceComponentId">>, Action, undefined) of
                    C when is_binary(C), C =/= <<>> -> <<" (component: ", C/binary, ")">>;
                    _ -> <<>>
                end,
    Context = beamai_utils:encode_json(
                case maps:get(<<"context">>, Action, #{}) of
                    M when is_map(M) -> M;
                    _ -> #{}
                end),
    <<"User performed action \"", Name/binary, "\" on surface \"", Surface/binary, "\"",
      Component/binary, ". Context: ", Context/binary>>.

%% @doc `beamai_agui_runner:run/4` 的 `input_transform`：把用户动作接进本轮输入
%%
%% 前端点按钮时发的那一轮**可能没有用户消息**（就是个动作），所以把动作合成一条
%% user 消息追加进去 —— 走的是与普通输入完全相同的路（`last_user_message/1` 取最后
%% 一条），没有新增机制。用户既打了字又点了按钮时，动作排在后面。
-spec input_transform() -> fun((beamai_agui_types:run_input()) ->
                                  beamai_agui_types:run_input()).
input_transform() ->
    fun(Input) ->
        case action_of(Input) of
            undefined ->
                Input;
            Action ->
                Text = user_action_message(Action),
                Messages = beamai_agui_types:messages(Input),
                Input#{messages => Messages ++ [#{<<"role">> => <<"user">>,
                                                  <<"content">> => Text}]}
        end
    end.

action_of(Input) ->
    case maps:get(<<"a2uiAction">>, beamai_agui_types:forwarded_props(Input), undefined) of
        A2ui when is_map(A2ui) ->
            case maps:get(<<"userAction">>, A2ui, undefined) of
                Action when is_map(Action) -> Action;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

%%====================================================================
%% 内部
%%====================================================================

surface_id(Args) ->
    get_str(Args, <<"surfaceId">>, <<"default">>).

get_str(Map, Key, Default) ->
    case maps:get(Key, Map, undefined) of
        V when is_binary(V), V =/= <<>> -> V;
        _ -> Default
    end.

%% @private 用法提示词 —— **这是提示词不是文档**：flat 组件数组、root 必须是布局
%% 组件、数据绑定语法、重复渲染语法、按钮 action 的形状，模型全靠这段学会。
guidelines(Tool) ->
    <<"## How to call ", Tool/binary, "\n\n"
      "You MUST provide ALL required arguments when calling ", Tool/binary, ":\n\n"
      "- **surfaceId** (string, required): Unique ID for the surface.\n"
      "- **components** (array, REQUIRED): A2UI v0.9 flat component array. NEVER omit this.\n"
      "- **data** (object, optional): Initial data model for path-bound component values.\n\n"
      "Note: the catalog id is set by the host, not by you. Do not include a catalogId argument.\n\n"
      "### Component format (v0.9 flat)\n\n"
      "Components are a flat array -- children are referenced by ID, not nested:\n"
      "- Every component has `id` (unique) and `component` (type name from the catalog).\n"
      "- The root component MUST have `id: \"root\"`.\n"
      "- Properties go directly on the component object.\n"
      "- Use `children: [\"id1\", \"id2\"]` for multiple children, `child: \"id\"` for one.\n\n"
      "### Minimal example\n\n"
      "```json\n"
      "{\n"
      "  \"surfaceId\": \"my-dashboard\",\n"
      "  \"components\": [\n"
      "    { \"id\": \"root\", \"component\": \"Column\", \"children\": [\"title\", \"row1\"] },\n"
      "    { \"id\": \"title\", \"component\": \"Text\", \"text\": \"Overview\", \"variant\": \"h2\" },\n"
      "    { \"id\": \"row1\", \"component\": \"Row\", \"children\": [\"c1\", \"c2\"] },\n"
      "    { \"id\": \"c1\", \"component\": \"Text\", \"text\": \"Users: 1,200\" },\n"
      "    { \"id\": \"c2\", \"component\": \"Text\", \"text\": \"Revenue: $50K\" }\n"
      "  ]\n"
      "}\n"
      "```\n\n"
      "### Key rules\n\n"
      "1. NEVER call ", Tool/binary, " without the `components` array -- the UI will be empty.\n"
      "2. Root must be a layout component (Column, Row, Card) -- not Text or Button.\n"
      "3. Component IDs must be unique. A component must NOT reference itself as child.\n"
      "4. Only use component names from the Available Components schema in context.\n"
      "5. For data binding use `{ \"path\": \"/key\" }` (absolute) or `{ \"path\": \"key\" }` "
      "(relative inside templates).\n"
      "6. For repeating content: `children: { componentId: \"card-id\", path: \"/items\" }`.\n"
      "7. Button actions: `\"action\": { \"event\": { \"name\": \"...\", \"context\": { ... } } }`.\n"
      "8. No placeholder images -- only use real URLs or Icon components.">>.

%%====================================================================
%% A2UI v0.9 基础 catalog
%%====================================================================

%% @doc A2UI v0.9 基础 catalog（18 个组件）
%%
%% 摘自 `@a2ui/web_core` 的基础 catalog：组件名 + 属性 + 属性描述 + 枚举 + 必填。
%% 去掉了 `$ref` 与公共部分 —— 那些是给校验器看的，模型只需要词汇表。
%% 要换成你自己的组件库，给 `with_tool/2` 传 `catalog`。
-spec basic_catalog() -> map().
basic_catalog() ->
    #{<<"catalogId">> => ?CATALOG_ID,
      <<"components">> => #{
        <<"Text">> => comp(#{<<"text">> => d(<<"The text content to display. Simple Markdown is supported.">>),
                             <<"variant">> => e(<<"A hint for the base text style.">>,
                                                [<<"h1">>, <<"h2">>, <<"h3">>, <<"h4">>,
                                                 <<"h5">>, <<"caption">>, <<"body">>])},
                            [<<"text">>]),
        <<"Image">> => comp(#{<<"url">> => d(<<"The URL of the image to display.">>),
                              <<"description">> => d(<<"Accessibility text for the image.">>),
                              <<"fit">> => e(<<"How the image is resized to fit its container.">>,
                                             [<<"contain">>, <<"cover">>, <<"fill">>,
                                              <<"none">>, <<"scaleDown">>]),
                              <<"variant">> => e(<<"A hint for the image size and style.">>,
                                                 [<<"icon">>, <<"avatar">>, <<"smallFeature">>,
                                                  <<"mediumFeature">>, <<"largeFeature">>,
                                                  <<"header">>])},
                            [<<"url">>]),
        <<"Icon">> => comp(#{<<"name">> => d(<<"The name of the icon to display.">>)},
                           [<<"name">>]),
        <<"Video">> => comp(#{<<"url">> => d(<<"The URL of the video to display.">>)},
                            [<<"url">>]),
        <<"AudioPlayer">> => comp(#{<<"url">> => d(<<"The URL of the audio to be played.">>),
                                    <<"description">> => d(<<"A title or summary of the audio.">>)},
                                  [<<"url">>]),
        <<"Row">> => comp(#{<<"children">> => d(<<"Array of child ids, or a template object.">>),
                            <<"justify">> => e(<<"Arrangement along the horizontal axis.">>,
                                               [<<"center">>, <<"end">>, <<"spaceAround">>,
                                                <<"spaceBetween">>, <<"spaceEvenly">>,
                                                <<"start">>, <<"stretch">>]),
                            <<"align">> => e(<<"Alignment along the vertical axis.">>,
                                             [<<"start">>, <<"center">>, <<"end">>,
                                              <<"stretch">>])},
                          [<<"children">>]),
        <<"Column">> => comp(#{<<"children">> => d(<<"Array of child ids, or a template object.">>),
                               <<"justify">> => e(<<"Arrangement along the vertical axis.">>,
                                                  [<<"start">>, <<"center">>, <<"end">>,
                                                   <<"spaceBetween">>, <<"spaceAround">>,
                                                   <<"spaceEvenly">>, <<"stretch">>]),
                               <<"align">> => e(<<"Alignment along the horizontal axis.">>,
                                                [<<"center">>, <<"end">>, <<"start">>,
                                                 <<"stretch">>])},
                             [<<"children">>]),
        <<"List">> => comp(#{<<"children">> => d(<<"Array of child ids, or a template object.">>),
                             <<"direction">> => e(<<"Layout direction of the list items.">>,
                                                  [<<"vertical">>, <<"horizontal">>]),
                             <<"align">> => e(<<"Alignment along the cross axis.">>,
                                              [<<"start">>, <<"center">>, <<"end">>,
                                               <<"stretch">>])},
                           [<<"children">>]),
        <<"Card">> => comp(#{<<"child">> => d(<<"The id of the single child component. "
                                               "Wrap multiple elements in a layout component.">>)},
                           [<<"child">>]),
        <<"Tabs">> => comp(#{<<"tabs">> => d(<<"Array of objects, each with a title and a child.">>)},
                           [<<"tabs">>]),
        <<"Modal">> => comp(#{<<"trigger">> => d(<<"The id of the component that opens the modal.">>),
                              <<"content">> => d(<<"The id of the component shown inside.">>)},
                            [<<"content">>, <<"trigger">>]),
        <<"Divider">> => comp(#{<<"axis">> => e(<<"The orientation of the divider.">>,
                                                [<<"horizontal">>, <<"vertical">>])}, []),
        <<"Button">> => comp(#{<<"child">> => d(<<"The id of the child component (usually a Text).">>),
                               <<"variant">> => e(<<"A hint for the button style.">>,
                                                  [<<"default">>, <<"primary">>, <<"borderless">>]),
                               <<"action">> => #{}},
                             [<<"action">>, <<"child">>]),
        <<"TextField">> => comp(#{<<"label">> => d(<<"The text label for the input field.">>),
                                  <<"value">> => d(<<"The value of the text field.">>),
                                  <<"variant">> => e(<<"The type of input field.">>,
                                                     [<<"longText">>, <<"number">>,
                                                      <<"shortText">>, <<"obscured">>]),
                                  <<"validationRegexp">> =>
                                      d(<<"Regular expression for client-side validation.">>)},
                                [<<"label">>]),
        <<"CheckBox">> => comp(#{<<"label">> => d(<<"The text next to the checkbox.">>),
                                 <<"value">> => d(<<"The current state (true / false).">>)},
                               [<<"label">>, <<"value">>]),
        <<"ChoicePicker">> => comp(#{<<"label">> => d(<<"The label for the group of options.">>),
                                     <<"variant">> => e(<<"How the picker behaves.">>,
                                                        [<<"multipleSelection">>,
                                                         <<"mutuallyExclusive">>]),
                                     <<"options">> => d(<<"The list of available options.">>),
                                     <<"value">> => d(<<"The currently selected values "
                                                        "(bind to a string array).">>),
                                     <<"displayStyle">> => e(<<"The display style.">>,
                                                             [<<"checkbox">>, <<"chips">>]),
                                     <<"filterable">> =>
                                         d(<<"If true, shows a search input to filter options.">>)},
                                   [<<"options">>, <<"value">>]),
        <<"Slider">> => comp(#{<<"label">> => d(<<"The label for the slider.">>),
                               <<"min">> => d(<<"The minimum value.">>),
                               <<"max">> => d(<<"The maximum value.">>),
                               <<"value">> => d(<<"The current value.">>)},
                             [<<"max">>, <<"value">>]),
        <<"DateTimeInput">> => comp(#{<<"value">> => d(<<"ISO 8601 value; start with \"\" if unset.">>),
                                      <<"enableDate">> => d(<<"If true, allows picking a date.">>),
                                      <<"enableTime">> => d(<<"If true, allows picking a time.">>),
                                      <<"min">> => d(<<"Minimum allowed date/time (ISO 8601).">>),
                                      <<"max">> => d(<<"Maximum allowed date/time (ISO 8601).">>),
                                      <<"label">> => d(<<"The text label for the input field.">>)},
                                    [<<"value">>])
      }}.

comp(Properties, []) ->
    #{<<"properties">> => Properties};
comp(Properties, Required) ->
    #{<<"properties">> => Properties, <<"required">> => Required}.

d(Description) ->
    #{<<"description">> => Description}.

e(Description, Enum) ->
    #{<<"description">> => Description, <<"enum">> => Enum}.

%%%-------------------------------------------------------------------
%%% @doc 脚本化的 LLM —— 让 AG-UI 的例子**离线可跑**
%%%
%%% AG-UI 的价值在事件流的形状：谁先谁后、成不成对、带不带 id。这些与模型说了
%%% 什么无关，却每次都要一个能说话的模型才验得了。所以这里给一个照剧本演的
%%% provider：你说第一轮调哪个工具、参数是什么，它就照着发 —— 于是
%%% `example_agui_full` 不用 API key 也跑得起来，事件序列还每次都一样。
%%%
%%% **它不是 mock 的替代品**：真 provider 的形状差异（OpenAI 系 / Anthropic 系）、
%%% 网络抖动、模型的临场发挥，这里一概没有。要验那些看
%%% `example_agui_live`（真 LLM）。这个模块只负责让「事件流的形状」这件事可以
%%% 被反复、确定地看一遍。
%%%
%%% == 用 ==
%%%
%%% ```erlang
%%% ok = example_agui_scripted:install([
%%%     %% 第一轮：调一个工具，参数**逐字**流出来（于是 TOOL_CALL_ARGS 是增量的）
%%%     {tool_call, <<"c1">>, <<"get_weather">>, <<"{\"city\":\"北京\"}">>},
%%%     %% 第二轮：一段思考 + 一句正文
%%%     {reasoning_then_text, <<"让我想想…">>, <<"北京今天晴。">>}
%%% ]),
%%% ...
%%% example_agui_scripted:uninstall().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agui_scripted).

-export([install/1, uninstall/0]).
-export([text/1, tool_call/3, reasoning_then_text/2, hang/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 装上剧本。第 N 次 LLM 调用演第 N 幕；演完了就一直演最后一幕。
%%
%% 每一幕：
%%
%% - `{text, Text}` —— 逐 token 吐一句正文；
%% - `{tool_call, Id, Name, ArgsJson}` —— 调一个工具，**参数逐字流出来**
%%   （这正是 `TOOL_CALL_ARGS` 增量那条路要验的东西）；
%% - `{reasoning_then_text, Thinking, Text}` —— 先一段思考（`REASONING_*`），
%%   再一句正文；
%% - `hang` —— 永不返回（用来验「停一个还在跑的 run」）。
-spec install([term()]) -> ok.
install(Script) ->
    Counter = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, RawCb, Opts) ->
            counters:add(Counter, 1, 1),
            play(scene(Script, counters:get(Counter, 1)), RawCb, Opts)
        end),
    %% 子 agent 走非流式（manager 的 do_run 用 beamai_agent:run/2）
    meck:expect(beamai_chat_model, chat,
        fun(_Config, _Messages, _Opts) ->
            {ok, beamai_chat_response:new(#{content => <<"子 agent 的结论。"/utf8>>,
                                            finish_reason => <<"stop">>})}
        end),
    ok.

-spec uninstall() -> ok.
uninstall() ->
    catch meck:unload(beamai_chat_model),
    ok.

%%====================================================================
%% 幕的构造器（可读性糖）
%%====================================================================

text(Text) -> {text, Text}.
tool_call(Id, Name, ArgsJson) -> {tool_call, Id, Name, ArgsJson}.
reasoning_then_text(Thinking, Text) -> {reasoning_then_text, Thinking, Text}.
hang() -> hang.

%%====================================================================
%% 内部
%%====================================================================

scene(Script, N) when N =< length(Script) -> lists:nth(N, Script);
scene(Script, _N) -> lists:last(Script).

play({text, Text}, _RawCb, Opts) ->
    emit_tokens(Text, Opts),
    {ok, beamai_chat_response:new(#{content => Text, finish_reason => <<"stop">>})};

play({reasoning_then_text, Thinking, Text}, RawCb, Opts) ->
    %% 思考走 raw 通道（provider 原样的 chunk），正文走 token 通道 ——
    %% 两条路本来就是分开的，例子里也别把它们混成一条
    _ = [RawCb(reasoning_chunk(C)) || C <- chars(Thinking)],
    emit_tokens(Text, Opts),
    {ok, beamai_chat_response:new(#{content => Text, finish_reason => <<"stop">>})};

play({tool_call, Id, Name, ArgsJson}, RawCb, _Opts) ->
    %% **逐字**流出参数：先开头（带 id 与名字），再一片一片的 arguments。
    %% 这是 `beamai_agui_raw` 那条增量路唯一的输入形状。
    _ = RawCb(tool_open_chunk(Id, Name)),
    _ = [RawCb(tool_args_chunk(C)) || C <- chars(ArgsJson)],
    {ok, beamai_chat_response:new(
           #{content => null,
             tool_calls => [#{id => Id, type => <<"function">>,
                              function => #{name => Name, arguments => ArgsJson}}],
             finish_reason => <<"tool_calls">>})};

play(hang, _RawCb, Opts) ->
    %% 先吐一个 token，好让调用方知道 run 真的跑起来了，再挂住
    emit_tokens(<<"思考中"/utf8>>, Opts),
    timer:sleep(120000),
    {ok, beamai_chat_response:new(#{content => <<>>, finish_reason => <<"stop">>})}.

emit_tokens(Text, Opts) ->
    case maps:get(on_llm_new_token, Opts, undefined) of
        Cb when is_function(Cb, 2) -> [Cb(C, #{}) || C <- chars(Text)], ok;
        _ -> ok
    end.

%% 按**字符**切（不是字节）—— 按字节切会把一个汉字劈成两半，打出来是乱码
chars(Bin) ->
    [unicode:characters_to_binary([C]) || C <- unicode:characters_to_list(Bin)].

%% OpenAI 系的 chunk 形状（`beamai_agui_raw` 认得它）
tool_open_chunk(Id, Name) ->
    #{<<"choices">> => [#{<<"delta">> =>
                              #{<<"tool_calls">> =>
                                    [#{<<"index">> => 0, <<"id">> => Id,
                                       <<"function">> => #{<<"name">> => Name}}]}}]}.

tool_args_chunk(Fragment) ->
    #{<<"choices">> => [#{<<"delta">> =>
                              #{<<"tool_calls">> =>
                                    [#{<<"index">> => 0,
                                       <<"function">> => #{<<"arguments">> => Fragment}}]}}]}.

reasoning_chunk(Fragment) ->
    #{<<"choices">> => [#{<<"delta">> => #{<<"reasoning_content">> => Fragment}}]}.

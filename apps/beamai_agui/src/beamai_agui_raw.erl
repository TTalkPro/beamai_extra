%%%-------------------------------------------------------------------
%%% @doc provider 原始流事件 → AG-UI 增量事件
%%%
%%% 上游的 `on_llm_event` 把 provider **原样解出的** SSE chunk 透出来（binary 键、
%%% 各家形状不同、不归一化）。归一化后的统一响应里看不到的东西全在这里：
%%%
%%%   - `tool_calls` 的 `arguments` **逐字增量** —— 前端工具参数的打字机效果
%%%   - thinking / reasoning 增量 —— `REASONING_*`
%%%
%%% 代价是必须按 provider 分支解读。这个模块就是那些分支，把它们收在一处，
%%% 事件的顺序与配对规则仍归 beamai_agui_emitter 管。
%%%
%%% == 支持的两种形状 ==
%%%
%%% **OpenAI 系**（openai / deepseek / zhipu / moonshot / xai / openrouter /
%%% siliconflow …，凡是 OpenAI 兼容口的都是这个形状）：
%%% ```
%%% #{<<"choices">> => [#{<<"delta">> => #{
%%%       <<"tool_calls">> => [#{<<"index">> => 0, <<"id">> => <<"c1">>,
%%%                              <<"function">> => #{<<"name">> => <<"f">>,
%%%                                                  <<"arguments">> => <<"{\\"a">>}}],
%%%       <<"reasoning_content">> => <<"嗯"/utf8>>}}]}
%%% ```
%%% 首个 delta 带 id 与 name，后续只带 index 与 arguments 片段 —— 所以必须记住
%%% index → {id, name} 的映射（这份状态在 emitter 里）。
%%%
%%% **Anthropic 系**（anthropic，以及走 anthropic 口的 ZHIPU / MiniMax）：
%%% ```
%%% #{<<"type">> => <<"content_block_start">>, <<"index">> => 1,
%%%   <<"content_block">> => #{<<"type">> => <<"tool_use">>,
%%%                            <<"id">> => <<"c1">>, <<"name">> => <<"f">>}}
%%% #{<<"type">> => <<"content_block_delta">>, <<"index">> => 1,
%%%   <<"delta">> => #{<<"type">> => <<"input_json_delta">>,
%%%                    <<"partial_json">> => <<"{\\"a">>}}
%%% ```
%%%
%%% 认不出来的 chunk 一律**安静丢弃**：这条通道是旁路增强，多认一种形状是加分，
%%% 认不出不该让主流程有任何感知。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_raw).

-export([translator/0]).

%% @doc 交给 beamai_agui_emitter 的 raw_translator
-spec translator() -> fun((map(), beamai_agui_emitter:emitter()) -> ok).
translator() ->
    fun(Event, Emitter) -> translate(Event, Emitter) end.

%% @private 翻译一个 provider 原始 chunk（认不出则 no-op）
-spec translate(term(), beamai_agui_emitter:emitter()) -> ok.
%% --- OpenAI 系 ---
translate(#{<<"choices">> := [#{<<"delta">> := Delta} | _]}, E) when is_map(Delta) ->
    ok = openai_reasoning(Delta, E),
    openai_tool_calls(maps:get(<<"tool_calls">>, Delta, undefined), E);
%% --- Anthropic 系 ---
translate(#{<<"type">> := <<"content_block_start">>,
            <<"index">> := Index,
            <<"content_block">> := #{<<"type">> := <<"tool_use">>,
                                     <<"id">> := Id, <<"name">> := Name}}, E) ->
    beamai_agui_emitter:tool_open(E, Index, Id, Name);
translate(#{<<"type">> := <<"content_block_delta">>,
            <<"index">> := Index,
            <<"delta">> := #{<<"partial_json">> := Partial}}, E)
  when is_binary(Partial) ->
    beamai_agui_emitter:tool_args(E, Index, Partial);
translate(#{<<"type">> := <<"content_block_delta">>,
            <<"delta">> := #{<<"thinking">> := Thinking}}, E)
  when is_binary(Thinking) ->
    beamai_agui_emitter:reasoning_delta(E, Thinking);
translate(_Event, _E) ->
    ok.

%%====================================================================
%% 内部 - OpenAI 系
%%====================================================================

%% deepseek / zhipu 用 reasoning_content，openrouter 用 reasoning
openai_reasoning(Delta, E) ->
    case first_binary([maps:get(<<"reasoning_content">>, Delta, undefined),
                       maps:get(<<"reasoning">>, Delta, undefined)]) of
        undefined -> ok;
        Text -> beamai_agui_emitter:reasoning_delta(E, Text)
    end.

openai_tool_calls(undefined, _E) ->
    ok;
openai_tool_calls(ToolCalls, E) when is_list(ToolCalls) ->
    lists:foreach(fun(TC) -> openai_tool_call(TC, E) end, ToolCalls);
openai_tool_calls(_Other, _E) ->
    ok.

%% 首个 delta 带 id/name（开一次调用），之后只带 arguments 片段
openai_tool_call(#{<<"index">> := Index} = TC, E) ->
    Fun = maps:get(<<"function">>, TC, #{}),
    ok = case {maps:get(<<"id">>, TC, undefined), maps:get(<<"name">>, Fun, undefined)} of
        {Id, Name} when is_binary(Id), is_binary(Name) ->
            beamai_agui_emitter:tool_open(E, Index, Id, Name);
        _ ->
            ok
    end,
    case maps:get(<<"arguments">>, Fun, undefined) of
        Args when is_binary(Args), Args =/= <<>> ->
            beamai_agui_emitter:tool_args(E, Index, Args);
        _ ->
            ok
    end;
openai_tool_call(_TC, _E) ->
    ok.

%%====================================================================
%% 内部 - 辅助
%%====================================================================

first_binary([B | _]) when is_binary(B), B =/= <<>> -> B;
first_binary([_ | Rest]) -> first_binary(Rest);
first_binary([]) -> undefined.

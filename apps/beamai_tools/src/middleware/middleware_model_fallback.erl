%%%-------------------------------------------------------------------
%%% @doc 模型降级中间件（around 模型）
%%%
%%% 当主模型调用失败时，自动切换到备选降级模型。
%%% around_chat 包裹 LLM 调用：调 Next → 检测错误 → 切换模型后重调 Next。
%%% 当所有降级模型耗尽时，返回原始错误。
%%%
%%% 触发条件以 beamai_llm_error 分类为准，比"可重试"更宽——换模型能救的都算：
%%%   - retryable（429 限流 / 5xx / 超时 / 连接断开）：换个模型可能不限流
%%%   - auth（401/403）：该 provider 密钥无效，降级模型可能在别的 provider
%%%   - 404：模型不存在，换模型
%%% 其余（如 400 请求本身有问题）换模型也救不了，不触发。
%%% trigger_errors 原子列表仅作兜底，匹配上游无法识别的自定义错误。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_fallback).

-behaviour(beamai_middleware).

-export([init/1, around_chat/3]).

%%====================================================================
%% 中间件回调
%%%-------------------------------------------------------------------

-spec init(map()) -> map().
init(Opts) ->
    #{
        fallback_models => maps:get(fallback_models, Opts, []),
        trigger_errors => maps:get(trigger_errors, Opts, default_trigger_errors()),
        on_fallback => maps:get(on_fallback, Opts, undefined),
        fallback_index => 0
    }.

%% @doc 包裹 LLM 调用：失败时依次切换到降级模型。
-spec around_chat(beamai_middleware:request(), map(), beamai_middleware:next()) ->
    beamai_middleware:response() | {beamai_middleware:response(), map()}.
around_chat(Req, State, Next) ->
    try_fallback(Req, State, Next, 0).

%%====================================================================
%% 内部函数
%%%-------------------------------------------------------------------

default_trigger_errors() ->
    [timeout, connection_error, connection_closed, rate_limit,
     rate_limited, server_error, service_unavailable,
     internal_server_error, bad_gateway, gateway_timeout,
     model_not_found, invalid_api_key, quota_exceeded].

%% @private 递归尝试降级模型
try_fallback(Req, State, Next, Index) ->
    #{fallback_models := FallbackModels,
      trigger_errors := TriggerErrors,
      on_fallback := OnFallback} = State,

    Resp = Next(Req),

    case extract_error(Resp) of
        undefined ->
            {Resp, State#{fallback_index => Index}};
        Error ->
            case should_trigger_fallback(Error, TriggerErrors) of
                false ->
                    {Resp, State#{fallback_index => Index}};
                true ->
                    NextIndex = Index + 1,
                    case NextIndex =< length(FallbackModels) of
                        true ->
                            FallbackModel = lists:nth(NextIndex, FallbackModels),
                            maybe_call_on_fallback(OnFallback, maps:get(model, maps:get(opts, Req, #{}), undefined), FallbackModel, Error),
                            %% 修改请求中的模型，重试
                            Opts = maps:get(opts, Req, #{}),
                            NewReq = Req#{opts => Opts#{model => FallbackModel}},
                            try_fallback(NewReq, State, Next, NextIndex);
                        false ->
                            %% 降级模型已耗尽
                            {Resp, State#{fallback_index => Index}}
                    end
            end
    end.

%% @private 从 chat 响应中提取错误
extract_error(#{response := {error, Error}}) -> Error;
extract_error(#{response := #{error := Error}}) -> Error;
extract_error(#{response := Error}) when is_atom(Error), Error =/= ok -> Error;
extract_error(_) -> undefined.

%% @private 先问上游分类器，识别不了再走兜底列表
should_trigger_fallback(Error, TriggerErrors) ->
    case classify(Error) of
        undefined -> matches_legacy(Error, TriggerErrors);
        LlmError ->
            case triggers_by_class(LlmError) of
                true -> true;
                false -> matches_legacy(Error, TriggerErrors)
            end
    end.

%% @private 换模型能救的错误类别
triggers_by_class(LlmError) ->
    beamai_llm_error:retryable(LlmError)
        orelse beamai_llm_error:type(LlmError) =:= auth
        orelse beamai_llm_error:status(LlmError) =:= 404.

%% @private 归一化为 beamai_llm_error；无法分类时返回 undefined（中间件不应因分类崩溃）
classify(Error) ->
    try beamai_llm_error:from_reason(Error)
    catch _:_ -> undefined
    end.

%% @private 兜底：匹配自定义原子/文本错误
matches_legacy({ErrorType, _}, TriggerErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, TriggerErrors);
matches_legacy(ErrorType, TriggerErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, TriggerErrors);
matches_legacy(ErrorBin, _) when is_binary(ErrorBin) ->
    LowerError = string:lowercase(binary_to_list(ErrorBin)),
    lists:any(fun(Pattern) ->
        string:find(LowerError, Pattern) =/= nomatch
    end, ["timeout", "rate limit", "connection", "server error",
          "503", "502", "504", "quota", "invalid key", "model not found"]);
matches_legacy(_, _) -> false.

maybe_call_on_fallback(undefined, _, _, _) -> ok;
maybe_call_on_fallback(OnFallback, FromModel, ToModel, Error) when is_function(OnFallback, 3) ->
    try OnFallback(FromModel, ToModel, Error)
    catch _:R -> logger:warning("Fallback callback error: ~p", [R])
    end.

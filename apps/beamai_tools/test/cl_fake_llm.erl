%%%-------------------------------------------------------------------
%%% @doc 测试桩：假 LLM provider。
%%%
%%% beamai_kernel 的 chat_terminal 用 `maps:get(module, LlmConfig, beamai_chat_completion)'
%%% 决定调谁，把它设成本模块即可在不发网络请求的前提下跑通 around_chat 链。
%%% @end
%%%-------------------------------------------------------------------
-module(cl_fake_llm).

-export([chat/3]).

chat(_Config, _Messages, _Opts) ->
    {ok, #{content => <<"real answer">>, tool_calls => [], finish_reason => stop}}.

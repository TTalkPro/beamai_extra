%%%-------------------------------------------------------------------
%%% @doc 测试桩：实现全部三个 around_* 回调。
%%%
%%% around_chat 返回 {Resp, NewState} 形式（验证状态回写路径），
%%% around_tool / around_turn 返回裸 Resp（验证无状态路径）。
%%% @end
%%%-------------------------------------------------------------------
-module(mw_stub_hooks).

-export([init/1, around_chat/3, around_tool/3, around_turn/3]).

init(Opts) -> Opts#{calls => 0}.

around_chat(Req, State, Next) ->
    Resp = Next(Req#{tagged_by => chat}),
    {Resp, State#{calls => maps:get(calls, State, 0) + 1}}.

around_tool(Req, _State, Next) ->
    Next(Req#{tagged_by => tool}).

around_turn(Req, _State, Next) ->
    Next(Req#{tagged_by => turn}).

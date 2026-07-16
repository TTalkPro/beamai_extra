%%%-------------------------------------------------------------------
%%% @doc 测试桩：around_turn 按 beamai_middleware 的契约返回 {Resp, NewState}。
%%%
%%% 这正是会踩到地雷的写法——turn 响应是 tuple，filter_chain 无法回写状态。
%%% 用于验证 beamai_middleware_runner 会丢弃 turn 钩子的状态、只透传 Resp。
%%% @end
%%%-------------------------------------------------------------------
-module(mw_stub_turn_stateful).

-export([init/1, around_turn/3]).

init(Opts) -> Opts#{turns => 0}.

around_turn(Req, State, Next) ->
    Resp = Next(Req),
    {Resp, State#{turns => maps:get(turns, State, 0) + 1}}.

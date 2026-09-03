%%%-------------------------------------------------------------------
%%% @doc AG-UI Runtime 应用入口
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_app).

-behaviour(application).

-export([start/2, stop/1]).

%% @doc 启动应用
start(_StartType, _StartArgs) ->
    beamai_agui_sup:start_link().

%% @doc 停止应用
stop(_State) ->
    ok.

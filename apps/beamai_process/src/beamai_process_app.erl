%%%-------------------------------------------------------------------
%%% @doc BeamAI Process 应用回调模块
%%%
%%% 负责启动 process 框架的 supervision tree：
%%% - beamai_process_pool: Process step worker 池（poolboy）
%%% - beamai_process_sup: Process runtime 动态 supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_app).

-behaviour(application).

%% Application 回调
-export([start/2, stop/1]).

%%====================================================================
%% Application 回调实现
%%====================================================================

%% @doc 启动应用
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    beamai_process_top_sup:start_link().

%% @doc 停止应用
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

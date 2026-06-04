%%%-------------------------------------------------------------------
%%% @doc BeamAI Process 顶层 Supervisor
%%%
%%% 管理 process 框架的核心进程（原 beamai_core_sup 中的 process 部分）：
%%% - beamai_process_pool: Process step worker 池（poolboy）
%%% - beamai_process_sup: Process runtime 动态 supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_top_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor 回调
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor 回调
%%====================================================================

%% @doc 初始化 supervisor
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    {ok, {SupFlags, get_children()}}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取子进程规格
get_children() ->
    ProcessChildren = case should_start_process_pool() of
        true -> [process_pool_spec()];
        false -> []
    end,
    ProcessChildren ++ [process_sup_spec()].

%% @private 默认池大小计算（CPU * 2）
default_pool_size() ->
    erlang:system_info(schedulers) * 2.

%% @private 判断是否需要启动 Process pool
should_start_process_pool() ->
    case application:get_env(beamai_process, process_pool_enabled, true) of
        false -> false;
        true -> code:which(poolboy) =/= non_existing
    end.

%% @private Process worker pool 规格 (poolboy)
process_pool_spec() ->
    DefaultSize = default_pool_size(),
    PoolSize = application:get_env(beamai_process, process_pool_size, DefaultSize),
    MaxOverflow = application:get_env(beamai_process, process_pool_max_overflow, DefaultSize * 2),
    PoolArgs = [
        {name, {local, beamai_process_pool}},
        {worker_module, beamai_process_worker},
        {size, PoolSize},
        {max_overflow, MaxOverflow},
        {strategy, fifo}
    ],
    poolboy:child_spec(beamai_process_pool, PoolArgs, []).

%% @private Process runtime supervisor 规格
process_sup_spec() ->
    #{
        id => beamai_process_sup,
        start => {beamai_process_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [beamai_process_sup]
    }.

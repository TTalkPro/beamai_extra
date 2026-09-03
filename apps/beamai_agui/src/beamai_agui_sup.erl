%%%-------------------------------------------------------------------
%%% @doc AG-UI Runtime 监督树
%%%
%%% ```
%%% beamai_agui_sup
%%% ├── beamai_agui_registry (worker)          %% agent 工厂注册表
%%% └── beamai_pause_store_ets|_dets (worker)  %% thread 的未决暂停快照
%%% ```
%%%
%%% run 本身**不进监督树**：一次 run 的生命周期绑在它的 HTTP 连接上
%%% （连接断了这次 run 就没有意义），由适配器 spawn_link，见
%%% beamai_agui_cowboy_handler。
%%%
%%% 跨请求活下来的不是 run 进程而是**中断态快照**：暂停时落进 pause store，
%%% 下个请求由新进程读回来接着跑。起哪一个（ETS / DETS / 完全不起）由
%%% beamai_agui_thread:backend/0 按应用配置决定，见那个模块的模块头。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc 启动 Supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 初始化 Supervisor
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    Registry = #{
        id => beamai_agui_registry,
        start => {beamai_agui_registry, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_agui_registry]
    },
    {ok, {SupFlags, [Registry | pause_store_child()]}}.

%% @private 内置 pause store（宿主给了自己的句柄就不起）
pause_store_child() ->
    case beamai_agui_thread:backend() of
        {external, _Handle} ->
            [];
        {dets, Name, File} ->
            [store_child(Name, beamai_pause_store_dets, [Name, #{file => File}])];
        {ets, Name} ->
            [store_child(Name, beamai_pause_store_ets, [Name])]
    end.

store_child(Name, Module, Args) ->
    #{id => Name,
      start => {Module, start_link, Args},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Module]}.

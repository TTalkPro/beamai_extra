%%%-------------------------------------------------------------------
%%% @doc AG-UI Runtime 监督树
%%%
%%% ```
%%% beamai_agui_sup
%%% ├── beamai_agui_registry (worker)          %% agent 工厂注册表
%%% └── beamai_pause_store_ets (worker)        %% thread 的未决暂停快照
%%% ```
%%%
%%% run 本身**不进监督树**：一次 run 的生命周期绑在它的 HTTP 连接上
%%% （连接断了这次 run 就没有意义），由适配器 spawn_link，见
%%% beamai_agui_cowboy_handler。
%%%
%%% 跨请求活下来的不是 run 进程而是**中断态快照**：暂停时落进 pause store，
%%% 下个请求由新进程读回来接着跑（见 beamai_agui_thread）。配了外部
%%% `pause_store` 句柄时这里就不再起内置的 ETS store。
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

%% @private 内置 ETS pause store（宿主给了自己的句柄就不起）
pause_store_child() ->
    case beamai_agui_thread:own_store() of
        false ->
            [];
        true ->
            Name = beamai_agui_thread:store_name(),
            [#{id => Name,
               start => {beamai_pause_store_ets, start_link, [Name]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [beamai_pause_store_ets]}]
    end.

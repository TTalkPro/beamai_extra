%%%-------------------------------------------------------------------
%%% @doc AG-UI Runtime 监督树
%%%
%%% ```
%%% beamai_agui_sup
%%% ├── beamai_agui_registry (worker)          %% agent 工厂注册表
%%% ├── beamai_agui_session_sup (supervisor)   %% 会话进程（动态）
%%% │   └── beamai_agui_session (worker) ×N    %% 一个 thread 一个
%%% ├── beamai_agui_sessions (worker)          %% threadId → 会话进程
%%% ├── beamai_agui_auth / _rate_limit         %% 默认关闭的安全门
%%% └── beamai_pause_store_ets|_dets (worker)  %% thread 的未决暂停快照
%%% ```
%%%
%%% **run 进程挂在会话下，不挂在 HTTP 连接上** —— 这正是会话层要补的那件事：
%%% 连接断了 run 照跑，客户端重连凭 `Last-Event-ID` 补缺口。见
%%% beamai_agui_session 模块头。（会话层没起来时 beamai_agui_cowboy_handler
%%% 退化成"run 绑在请求上"的老行为，少一层能力而不是坏掉。）
%%%
%%% 起的顺序有意义：`beamai_agui_sessions` 建会话要 `supervisor:start_child`，
%%% 故 `beamai_agui_session_sup` 必须排在它前面。
%%%
%%% 会话是**进程内**的，节点一重启就没了。跨重启活下来的是**中断态快照**：
%%% 暂停时落进 pause store，下个请求由新进程读回来接着跑。起哪一个
%%% （ETS / DETS / 完全不起）由 beamai_agui_thread:backend/0 按应用配置决定。
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
    SessionSup = #{
        id => beamai_agui_session_sup,
        start => {beamai_agui_session_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [beamai_agui_session_sup]
    },
    Sessions = #{
        id => beamai_agui_sessions,
        start => {beamai_agui_sessions, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_agui_sessions]
    },
    Auth = #{
        id => beamai_agui_auth,
        start => {beamai_agui_auth, start_link,
                  [app_env(beamai_agui_auth)]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_agui_auth]
    },
    RateLimit = #{
        id => beamai_agui_rate_limit,
        start => {beamai_agui_rate_limit, start_link,
                  [app_env(beamai_agui_rate_limit)]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_agui_rate_limit]
    },
    Children = [Registry, SessionSup, Sessions, Auth, RateLimit |
                pause_store_child()],
    {ok, {SupFlags, Children}}.

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

%% @private 安全服务的启动配置走应用环境；不配就保持两道门关闭。
app_env(Name) ->
    application:get_env(beamai_agui, Name, #{}).

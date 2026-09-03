%%%-------------------------------------------------------------------
%%% @doc Agent 注册表（对标 CopilotRuntime 的 `agents` 字典）
%%%
%%% 存的是**工厂**而不是 agent 实例：agent 状态是纯数据（beamai_agent:new/1
%%% 返回 map），每次 run 都要一份带着本次 run 回调的新实例，共享实例只会串味。
%%%
%%% 工厂签名：
%%% ```erlang
%%% fun((RunInput :: beamai_agui_types:run_input(),
%%%      RuntimeCfg :: map()) -> {ok, AgentState} | {error, term()})
%%% ```
%%%
%%% `RuntimeCfg` **必须整个 merge 进 `beamai_agent:new/1` 的 config**，里面是
%%% runtime 替这次 run 决定的三件事：
%%%
%%% - `callbacks` —— 事件流的来源，不传就是个哑巴 agent；
%%% - `conversation_id` —— 对齐 AG-UI 的 threadId，跨请求的历史与暂停都靠它索引；
%%% - `pause_store` —— 中断态落库的去处（store 不可用时这个键不出现）。
%%%
%%% 典型写法：
%%% ```erlang
%%% fun(_Input, RuntimeCfg) ->
%%%     beamai_agent:new(maps:merge(#{chat_client => K}, RuntimeCfg))
%%% end
%%% ```
%%%
%%% 工厂必须**可重放**：同一个 thread 的两次请求要造出配置一致的 agent
%%% （同样的工具集、同样的 system prompt），否则从 pause_store 恢复的中断态
%%% 会对不上新 agent 的工具表。见 beamai_agui_thread 模块头。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_registry).

-behaviour(gen_server).

-export([start_link/0]).
-export([register/2, unregister/1, lookup/1, list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(TABLE, beamai_agui_agents).

-type factory() :: fun((beamai_agui_types:run_input(),
                        beamai_agent_callbacks:callbacks()) ->
                           {ok, map()} | {error, term()}).

-export_type([factory/0]).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 注册（或覆盖）一个 agent 工厂
-spec register(binary(), factory()) -> ok | {error, term()}.
register(Name, Factory) when is_binary(Name), is_function(Factory, 2) ->
    gen_server:call(?MODULE, {register, Name, Factory});
register(_Name, _Factory) ->
    {error, badarg}.

%% @doc 注销
-spec unregister(binary()) -> ok.
unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%% @doc 查一个 agent 工厂（读走 ETS，不经 gen_server）
-spec lookup(binary()) -> {ok, factory()} | {error, not_found}.
lookup(Name) ->
    try ets:lookup(?TABLE, Name) of
        [{Name, Factory}] -> {ok, Factory};
        [] -> {error, not_found}
    catch
        error:badarg -> {error, not_found}
    end.

%% @doc 列出已注册的 agent 名
-spec list() -> [binary()].
list() ->
    try
        lists:sort([N || {N, _} <- ets:tab2list(?TABLE)])
    catch
        error:badarg -> []
    end.

%%====================================================================
%% gen_server 回调
%%====================================================================

init([]) ->
    _ = ets:new(?TABLE, [named_table, set, protected, {read_concurrency, true}]),
    {ok, #{}}.

handle_call({register, Name, Factory}, _From, State) ->
    true = ets:insert(?TABLE, {Name, Factory}),
    {reply, ok, State};
handle_call({unregister, Name}, _From, State) ->
    true = ets:delete(?TABLE, Name),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

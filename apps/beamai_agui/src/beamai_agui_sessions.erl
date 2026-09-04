%%%-------------------------------------------------------------------
%%% @doc 会话注册表：threadId → 会话进程
%%%
%%% 会话进程的名字是 binary（threadId），`erlang:register/2` 只收 atom，所以自己
%%% 维护一张表。读走 ETS（不经 gen_server，`subscribe` / `stop` 这些路径每个请求
%%% 都要查一次），**建**走 gen_server —— 建立必须串行化，否则两个并发请求会给同
%%% 一个 thread 起出两个会话，事件流当场分叉。
%%%
%%% == 没起来就是没有会话层 ==
%%%
%%% `ensure/1` 在注册表没起来时返回 `{error, unavailable}`，**不崩**。宿主可能只
%%% 引了几个模块而没启 `beamai_agui` 应用（既有的单测就是这么做的），那时 HTTP
%%% 层退化成"run 的寿命绑在请求上"的老行为 —— 少一层能力，不是坏掉。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_sessions).

-behaviour(gen_server).

-export([start_link/0]).
-export([ensure/1, whereis_session/1, list/0, forget/1, count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(TABLE, beamai_agui_sessions).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 取这个 thread 的会话，没有就建一个
-spec ensure(binary()) -> {ok, pid()} | {error, term()}.
ensure(ThreadId) when is_binary(ThreadId) ->
    case whereis_session(ThreadId) of
        {ok, _Pid} = Found ->
            Found;
        {error, not_found} ->
            case erlang:whereis(?MODULE) of
                undefined -> {error, unavailable};
                _ -> gen_server:call(?MODULE, {ensure, ThreadId}, 5000)
            end;
        {error, _} = Error ->
            Error
    end;
ensure(_) ->
    {error, badarg}.

%% @doc 查（不建）
-spec whereis_session(binary()) -> {ok, pid()} | {error, not_found | unavailable}.
whereis_session(ThreadId) ->
    try ets:lookup(?TABLE, ThreadId) of
        [{ThreadId, Pid}] ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, not_found}
            end;
        [] ->
            {error, not_found}
    catch
        error:badarg -> {error, unavailable}
    end.

%% @doc 现有的会话（thread id）
-spec list() -> [binary()].
list() ->
    try
        lists:sort([T || {T, _} <- ets:tab2list(?TABLE)])
    catch
        error:badarg -> []
    end.

-spec count() -> non_neg_integer().
count() ->
    length(list()).

%% @doc 把一个会话从注册表里摘掉（在跑的 run 一并停掉）
%%
%% **不动 pause_store，也不动 chat memory** —— 历史归历史，未决的暂停归 store。
%% 摘掉的只是"进程内的那份缓冲与订阅"，下一个请求会重新建一个会话接上。
-spec forget(binary()) -> ok.
forget(ThreadId) ->
    case whereis_session(ThreadId) of
        {ok, Pid} ->
            _ = beamai_agui_session:stop_run(Pid),
            _ = gen_server:call(?MODULE, {forget, ThreadId}, 5000),
            ok;
        _ ->
            ok
    end.

%%====================================================================
%% gen_server 回调
%%====================================================================

init([]) ->
    _ = ets:new(?TABLE, [named_table, set, protected, {read_concurrency, true}]),
    {ok, #{monitors => #{}}}.

handle_call({ensure, ThreadId}, _From, State) ->
    %% 复查一次：排队期间别人可能已经建好了
    case whereis_session(ThreadId) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        _ ->
            case supervisor:start_child(beamai_agui_session_sup, [ThreadId]) of
                {ok, Pid} ->
                    true = ets:insert(?TABLE, {ThreadId, Pid}),
                    MRef = erlang:monitor(process, Pid),
                    Monitors = maps:put(MRef, ThreadId, maps:get(monitors, State)),
                    {reply, {ok, Pid}, State#{monitors => Monitors}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({forget, ThreadId}, _From, State) ->
    case whereis_session(ThreadId) of
        {ok, Pid} ->
            true = ets:delete(?TABLE, ThreadId),
            _ = supervisor:terminate_child(beamai_agui_session_sup, Pid),
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 会话自己走了（空闲驱逐、或者崩了）：把表里那条清掉
handle_info({'DOWN', MRef, process, _Pid, _Reason}, #{monitors := Monitors} = State) ->
    case maps:take(MRef, Monitors) of
        {ThreadId, Rest} ->
            true = ets:delete(?TABLE, ThreadId),
            {noreply, State#{monitors => Rest}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

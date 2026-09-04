%%%-------------------------------------------------------------------
%%% @doc 会话进程的动态监督者
%%%
%%% `simple_one_for_one`，`restart => temporary`：**会话不重启**。它里面全是易失
%%% 的东西 —— 事件环形缓冲、订阅者列表、seq 水位；重启出来的是一个空壳，订阅者
%%% 早随连接走了，而 seq 从 -1 重新开始会让还没断线的客户端"补 3 号之后的"补出
%%% 上一轮的事件。跨请求活下来的东西在 pause_store 与 chat memory 里，不在这儿。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_session_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 60},
    Child = #{id => beamai_agui_session,
              start => {beamai_agui_session, start_link, []},
              restart => temporary,
              shutdown => 5000,
              type => worker,
              modules => [beamai_agui_session]},
    {ok, {SupFlags, [Child]}}.

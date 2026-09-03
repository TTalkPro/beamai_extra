%%%-------------------------------------------------------------------
%%% @doc 重启后接着聊：pause_store + chat memory 双双落 DETS
%%%
%%% `example_agui_live` 证明的是「一次 run 能跑通」。这个例子回答另一个问题：
%%% **进程死过一次之后，这个会话还在不在**。
%%%
%%% 两条 DETS 文件各管一半，缺一不可：
%%%
%%%   - `pauses.dets`  —— 中断态（停在哪个 tool_call 上）
%%%   - `history.dets` —— 对话历史（之前说过什么）
%%%
%%% 只持久化前者，恢复出来的会话有中断态没有历史，模型看不到上下文；只持久化
%%% 后者，卡在半路的 HITL 接不上。
%%%
%%% == 用法：两个独立的 BEAM 进程 ==
%%%
%%% 这才是真的"重启"——不是同一个 VM 里 stop 再 start，是换一个 OS 进程：
%%%
%%% ```
%%% cd examples && make compile
%%% D=/tmp/agui_durable_demo
%%% E="../_build/default/lib:../_build/default/checkouts"
%%% P="_build/default/lib/beamai_examples/ebin"
%%% ERL_LIBS=$E erl -noshell -pa $P -eval "example_agui_durable:phase1(\"$D\")"
%%% ERL_LIBS=$E erl -noshell -pa $P -eval "example_agui_durable:phase2(\"$D\")"
%%% ```
%%%
%%% phase1 告诉 agent 一件事，进程退出；phase2 是**全新的 BEAM**，只靠磁盘上
%%% 那两个文件把会话接回来，然后问它刚才说了什么。
%%%
%%% 环境变量：`MINIMAX_API_KEY` / `ZHIPU_API_KEY`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agui_durable).

-export([phase1/1, phase1/2, phase2/1, phase2/2]).

-define(LISTENER, example_agui_durable_listener).
-define(AGENT, <<"memo-agent">>).
-define(THREAD, <<"durable-thread">>).
-define(PAUSE_STORE, agui_demo_pauses).
-define(HISTORY_STORE, agui_demo_history).

%%====================================================================
%% 两个阶段
%%====================================================================

phase1(Dir) -> phase1(Dir, minimax).

phase1(Dir, Provider) ->
    Port = setup(Dir, Provider, fresh),
    io:format("~n=== phase 1（首次启动）===~n"),
    io:format("~n--- 提问 ---~n  ~ts~n",
              [<<"我叫小李，项目代号是 Falcon。记住这两件事，回一句「记住了」。"/utf8>>]),
    Text = ask(Port, <<"我叫小李，项目代号是 Falcon。记住这两件事，回一句「记住了」。"/utf8>>),
    io:format("~n--- 答复 ---~n  ~ts~n", [Text]),
    io:format("~n磁盘上现在有：~n"),
    _ = [io:format("  ~ts  ~p bytes~n", [F, filelib:file_size(F)]) || F <- files(Dir)],
    io:format("~n进程即将退出。~n"),
    halt(0).

phase2(Dir) -> phase2(Dir, minimax).

phase2(Dir, Provider) ->
    Port = setup(Dir, Provider, existing),
    io:format("~n=== phase 2（全新 BEAM，只有磁盘上那两个文件）===~n"),
    %% 两个文件各管一半：这里读历史，下面看有没有未决暂停
    History = beamai_chat_memory:mem_get(history_handle(), ?THREAD),
    io:format("~n--- 从 history.dets 读回的历史（~p 条）---~n", [length(History)]),
    _ = [io:format("  ~-10s ~ts~n", [maps:get(role, M), brief(M)]) || M <- History],
    Q = <<"我叫什么？项目代号是什么？"/utf8>>,
    io:format("~n--- 提问 ---~n  ~ts~n", [Q]),
    Text = ask(Port, Q),
    io:format("~n--- 答复 ---~n  ~ts~n", [Text]),
    io:format("~n--- pause_store ---~n  未决暂停：~p"
              "（本例没触发 HITL，跨重启的中断恢复见 beamai_agui_durable_tests）~n",
              [beamai_agui_thread:pending(?THREAD)]),
    io:format("~n"),
    case {binary:match(Text, <<"小李"/utf8>>), binary:match(Text, <<"Falcon">>)} of
        {nomatch, _} -> io:format("!! 答复里没有「小李」——历史没接上~n");
        {_, nomatch} -> io:format("!! 答复里没有「Falcon」——历史没接上~n");
        _ -> io:format("OK 重启后会话接上了：名字与代号都还记得~n")
    end,
    halt(0).

%% 按**字符**截断而不是字节：binary:part/3 会把一个 UTF-8 码点劈成两半，
%% 打出来就是一串乱码
brief(#{content := C}) when is_binary(C) ->
    case string:length(C) > 48 of
        true -> unicode:characters_to_binary([string:slice(C, 0, 48), "..."]);
        false -> C
    end;
brief(_) -> <<"(null)">>.

%%====================================================================
%% 搭台
%%====================================================================

setup(Dir, Provider, Mode) ->
    ok = filelib:ensure_path(Dir),
    PauseFile = filename:join(Dir, "pauses.dets"),
    HistoryFile = filename:join(Dir, "history.dets"),
    case Mode of
        fresh -> _ = [file:delete(F) || F <- [PauseFile, HistoryFile]], ok;
        existing -> ok
    end,
    %% pause_store 必须在 beamai_agui 启动**之前**配好：sup 的 init 读它决定
    %% 起哪种后端（见 beamai_agui_thread:backend/0）
    application:set_env(beamai_agui, pause_store_name, ?PAUSE_STORE),
    application:set_env(beamai_agui, pause_store_file, PauseFile),
    {ok, _} = application:ensure_all_started(beamai_agui),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    %% chat memory 是 agent 的配置，由工厂自己持有（runtime 不越俎代庖）
    {ok, _} = beamai_chat_memory_dets:start_link(?HISTORY_STORE,
                                                 #{file => HistoryFile}),
    ok = beamai_agui_registry:register(?AGENT, factory(Provider)),
    start_http().

files(Dir) ->
    [F || F <- [filename:join(Dir, "pauses.dets"), filename:join(Dir, "history.dets")],
          filelib:is_regular(F)].

history_handle() ->
    beamai_chat_memory_dets:handle(?HISTORY_STORE).

factory(Provider) ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              llm(Provider)),
        beamai_agent:new(maps:merge(
            #{chat_client => K,
              %% 历史落 DETS —— 与 pause_store 各管一半，缺一不可
              memory => {store, history_handle()},
              system_prompt => <<"你是记事助手，回答简短。"/utf8>>},
            RuntimeCfg))
    end.

llm(minimax) ->
    example_llm_config:minimax(#{api_key => env("MINIMAX_API_KEY"),
                                 model => <<"m2.7">>});
llm(zhipu) ->
    example_llm_config:anthropic(#{api_key => env("ZHIPU_API_KEY"),
                                   model => <<"glm-5.3-flash">>}).

env(Name) ->
    case os:getenv(Name) of
        false -> error({missing_env, Name});
        V -> list_to_binary(V)
    end.

start_http() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/agui/run", beamai_agui_cowboy_handler, #{agent => ?AGENT}}]}
    ]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    ranch:get_port(?LISTENER).

%%====================================================================
%% 发一轮对话，返回拼好的答复文本
%%====================================================================

ask(Port, Prompt) ->
    Body = beamai_utils:encode_json(
             #{<<"threadId">> => ?THREAD,
               <<"messages">> => [#{<<"role">> => <<"user">>,
                                    <<"content">> => Prompt}]}),
    {ok, Conn} = gun:open("127.0.0.1", Port, #{retry => 0}),
    {ok, http} = gun:await_up(Conn, 5000),
    try
        Ref = gun:post(Conn, "/agui/run",
                       [{<<"content-type">>, <<"application/json">>}], Body),
        {response, nofin, 200, _} = gun:await(Conn, Ref, 30000),
        Events = stream(Conn, Ref, <<>>, []),
        iolist_to_binary([maps:get(<<"delta">>, E) || E <- Events,
                          maps:get(<<"type">>, E) =:= <<"TEXT_MESSAGE_CONTENT">>])
    after
        gun:close(Conn)
    end.

stream(Conn, Ref, Buf, Acc) ->
    case gun:await(Conn, Ref, 120000) of
        {data, nofin, Chunk} ->
            {Rest, Evs} = decode(<<Buf/binary, Chunk/binary>>),
            stream(Conn, Ref, Rest, Acc ++ Evs);
        {data, fin, Chunk} ->
            {_Rest, Evs} = decode(<<Buf/binary, Chunk/binary>>),
            Acc ++ Evs;
        {error, Reason} ->
            io:format("  !! stream error: ~p~n", [Reason]),
            Acc
    end.

decode(Data) ->
    {Rest, Frames} = beamai_sse:parse(Data),
    {Rest, [beamai_utils:parse_json(maps:get(data, F)) || F <- Frames]}.

%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_client_registry 的行为测试。
%%%
%%% 注册表是宿主管理多个 MCP client 的唯一入口，此前零覆盖。这里覆盖
%%% 注册 / 重名拒绝 / 注销 / 名字与 pid 两种查找键 / 标签过滤 / 进程死了
%%% 自动摘除，外加 start_and_register 的失败清理路径。
%%%
%%% 用例全部按**相对基线**断言：注册表挂在 beamai_mcp 监督树下，同一个 eunit
%%% 运行里别的 suite 可能已经往里塞过 client，所以这里不假设 count() 从 0 开始，
%%% 只断言自己新增的那几条。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_client_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 夹具
%%====================================================================

%% 复用已在运行的注册表（应用一起来就启动了），并在用例之间清掉自己注册的项。
setup() ->
    {ok, _} = application:ensure_all_started(beamai_mcp),
    case whereis(beamai_mcp_client_registry) of
        undefined ->
            {ok, Pid} = beamai_mcp_client_registry:start_link(),
            Pid;
        Pid ->
            Pid
    end,
    beamai_mcp_client_registry:count().

cleanup(_Baseline) ->
    lists:foreach(
      fun(#{name := <<"t_", _/binary>> = Name, pid := Pid}) ->
              catch beamai_mcp_client_registry:unregister(Name),
              catch beamai_mcp_client:stop(Pid);
         (_) ->
              ok
      end,
      beamai_mcp_client_registry:list_clients()),
    ok.

%% 本模块注册的名字统一前缀，cleanup 只清自己的，不碰别人的。
is_mcp_test_name(<<"t_", _/binary>>) -> true;
is_mcp_test_name(_) -> false.

all_names() ->
    [maps:get(name, Info) || Info <- beamai_mcp_client_registry:list_clients()].

registry_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun register_and_find/1,
      fun client_info_keeps_metadata/1,
      fun find_by_pid/1,
      fun duplicate_name_is_rejected/1,
      fun unregister_by_name_and_pid/1,
      fun lookup_missing_name/1,
      fun find_by_tag/1,
      fun dead_client_is_dropped/1,
      fun start_and_register_failure_cleans_up/1]}.

%%====================================================================
%% 用例
%%====================================================================

register_and_find(Base) ->
    fun() ->
        ?assertEqual(false, beamai_mcp_client_registry:is_registered(<<"t_fs">>)),
        ok = beamai_mcp_client_registry:register(<<"t_fs">>, self()),
        ?assertEqual(true, beamai_mcp_client_registry:is_registered(<<"t_fs">>)),
        ?assertEqual(true, beamai_mcp_client_registry:is_registered(self())),
        {ok, Client} = beamai_mcp_client_registry:get_client(<<"t_fs">>),
        ?assertEqual(self(), Client),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count()),
        ?assert(lists:member(self(), beamai_mcp_client_registry:list_client_pids()))
    end.

%% client_info 要带回注册时的元数据，否则宿主查了也白查。
client_info_keeps_metadata(Base) ->
    fun() ->
        ok = beamai_mcp_client_registry:register(<<"t_mem">>, self(), #{
            server_info => #{name => <<"memory">>},
            tags => [kv],
            metadata => #{region => <<"cn-1">>}
        }),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count()),
        {ok, Info} = beamai_mcp_client_registry:get_client_info(<<"t_mem">>),
        ?assertEqual(self(), maps:get(pid, Info)),
        ?assertEqual(<<"t_mem">>, maps:get(name, Info)),
        ?assertEqual(#{name => <<"memory">>}, maps:get(server_info, Info)),
        ?assertEqual([kv], maps:get(tags, Info)),
        ?assertEqual(#{region => <<"cn-1">>}, maps:get(metadata, Info)),
        ?assert(is_integer(maps:get(registered_at, Info)))
    end.

%% pid 也是合法查找键（get_client_info / is_registered 都收）。
find_by_pid(Base) ->
    fun() ->
        ok = beamai_mcp_client_registry:register(<<"t_pid">>, self()),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count()),
        {ok, Info} = beamai_mcp_client_registry:get_client_info(self()),
        ?assertEqual(<<"t_pid">>, maps:get(name, Info))
    end.

duplicate_name_is_rejected(Base) ->
    fun() ->
        ok = beamai_mcp_client_registry:register(<<"t_dup">>, self()),
        ?assertEqual({error, {already_registered, <<"t_dup">>}},
                     beamai_mcp_client_registry:register(<<"t_dup">>, self())),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count())
    end.

unregister_by_name_and_pid(Base) ->
    fun() ->
        ok = beamai_mcp_client_registry:register(<<"t_a">>, self()),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count()),
        ?assertEqual(ok, beamai_mcp_client_registry:unregister(<<"t_a">>)),
        ?assertEqual(Base, beamai_mcp_client_registry:count()),
        ?assertEqual({error, not_found}, beamai_mcp_client_registry:unregister(<<"t_a">>)),
        ?assertEqual(false, beamai_mcp_client_registry:is_registered(self())),
        %% 再用 pid 注销一次也不该出错（已经摘掉了）
        ?assertEqual({error, not_found}, beamai_mcp_client_registry:unregister(self())),
        ?assertEqual(Base, beamai_mcp_client_registry:count()),
        %% 同一个 pid 换个名字重新注册，注销按名字走
        ok = beamai_mcp_client_registry:register(<<"t_b">>, self()),
        ?assertEqual(ok, beamai_mcp_client_registry:unregister(<<"t_b">>)),
        ?assertEqual(Base, beamai_mcp_client_registry:count())
    end.

lookup_missing_name(_Base) ->
    fun() ->
        ?assertEqual({error, not_found},
                     beamai_mcp_client_registry:get_client(<<"t_nope">>)),
        ?assertEqual({error, not_found},
                     beamai_mcp_client_registry:get_client_info(<<"t_nope">>)),
        ?assertEqual(false, beamai_mcp_client_registry:is_registered(<<"t_nope">>)),
        ?assertEqual([], [N || N <- all_names(), is_mcp_test_name(N)])
    end.

find_by_tag(Base) ->
    fun() ->
        ok = beamai_mcp_client_registry:register(<<"t_tag1">>, self(), #{tags => [file]}),
        ok = beamai_mcp_client_registry:register(<<"t_tag2">>, self(), #{tags => [kv, file]}),
        ok = beamai_mcp_client_registry:register(<<"t_tag3">>, self(), #{tags => [kv]}),
        ?assertEqual(Base + 3, beamai_mcp_client_registry:count()),
        Found = [maps:get(name, I) ||
                 I <- beamai_mcp_client_registry:get_clients_by_tag(file)],
        ?assertEqual([<<"t_tag1">>, <<"t_tag2">>],
                     lists:sort([N || N <- Found, is_mcp_test_name(N)])),
        ?assertEqual([], beamai_mcp_client_registry:get_clients_by_tag(no_such_tag))
    end.

%% 客户端进程死了，注册表不能留一条僵尸记录：monitor 的 DOWN 要把它摘掉。
dead_client_is_dropped(Base) ->
    fun() ->
        Client = start_dummy_client(),
        ok = beamai_mcp_client_registry:register(<<"t_tmp">>, Client),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count()),
        ?assertEqual(true, beamai_mcp_client_registry:is_registered(<<"t_tmp">>)),
        exit(Client, kill),
        timer:sleep(50),
        ?assertEqual(false, beamai_mcp_client_registry:is_registered(<<"t_tmp">>)),
        ?assertEqual({error, not_found},
                     beamai_mcp_client_registry:get_client(<<"t_tmp">>)),
        ?assertEqual(Base, beamai_mcp_client_registry:count())
    end.

%% start_and_register 会真去 start_link 一个客户端。用一定连不上的配置触发失败
%% 分支，重点是**不能留下半注册状态**。
start_and_register_failure_cleans_up(Base) ->
    fun() ->
        {ok, Client} = beamai_mcp_client_registry:start_and_register(
                         <<"t_started">>, #{transport => stdio, auto_connect => false}),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count()),
        {error, {register_failed, {already_registered, <<"t_started">>}}} =
            beamai_mcp_client_registry:start_and_register(
              <<"t_started">>, #{transport => stdio, auto_connect => false}),
        ?assert(is_process_alive(Client)),
        ?assertEqual(Base + 1, beamai_mcp_client_registry:count()),
        ?assertEqual(true, beamai_mcp_client_registry:is_registered(<<"t_started">>)),
        ok = beamai_mcp_client_registry:unregister(<<"t_started">>),
        ok = beamai_mcp_client:stop(Client),
        ?assertEqual(Base, beamai_mcp_client_registry:count())
    end.

start_dummy_client() ->
    spawn(fun() -> receive stop -> ok end end).

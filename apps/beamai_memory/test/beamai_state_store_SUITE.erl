%%%-------------------------------------------------------------------
%%% @doc State Store 测试套件
%%%
%%% 测试 Layer 1 通用状态存储功能。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_state_store_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% CT 回调
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% 测试用例
-export([
    test_new/1,
    test_save_and_load/1,
    test_delete/1,
    test_exists/1,
    test_batch_operations/1,
    test_list_by_owner/1,
    test_list_by_branch/1,
    test_count/1
]).

%%====================================================================
%% CT 回调
%%====================================================================

all() ->
    [
        test_new,
        test_save_and_load,
        test_delete,
        test_exists,
        test_batch_operations,
        test_list_by_owner,
        test_list_by_branch,
        test_count
    ].

init_per_suite(Config) ->
    application:ensure_all_started(beamai_memory),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% 为每个测试创建新的 ETS store
    StoreName = list_to_atom("test_store_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {ok, _} = beamai_store_ets:start_link(StoreName, #{}),
    Backend = {beamai_store_ets, StoreName},
    Store = beamai_state_store:new(Backend, #{namespace => <<"test_state">>}),
    [{store, Store}, {store_name, StoreName} | Config].

end_per_testcase(_TestCase, Config) ->
    StoreName = ?config(store_name, Config),
    catch gen_server:stop(StoreName),
    ok.

%%====================================================================
%% 测试用例
%%====================================================================

%% @doc 测试创建 State Store
test_new(Config) ->
    Store = ?config(store, Config),
    ?assertMatch(#{backend := _, namespace := <<"test_state">>}, Store).

%% @doc 测试保存和加载
test_save_and_load(Config) ->
    Store = ?config(store, Config),

    %% 创建条目
    Entry = #state_entry{
        id = <<"entry_1">>,
        owner_id = <<"owner_1">>,
        parent_id = undefined,
        branch_id = <<"main">>,
        version = 1,
        state = #{key => value},
        entry_type = test_entry,
        created_at = erlang:system_time(millisecond),
        metadata = #{tag => test}
    },

    %% 保存
    {ok, SavedEntry} = beamai_state_store:save(Store, Entry),
    ?assertEqual(<<"entry_1">>, SavedEntry#state_entry.id),

    %% 加载
    {ok, LoadedEntry} = beamai_state_store:load(Store, <<"entry_1">>),
    ?assertEqual(<<"entry_1">>, LoadedEntry#state_entry.id),
    ?assertEqual(<<"owner_1">>, LoadedEntry#state_entry.owner_id),
    ?assertEqual(#{key => value}, LoadedEntry#state_entry.state).

%% @doc 测试删除
test_delete(Config) ->
    Store = ?config(store, Config),

    %% 创建并保存条目
    Entry = #state_entry{
        id = <<"entry_del">>,
        owner_id = <<"owner_1">>,
        parent_id = undefined,
        branch_id = <<"main">>,
        version = 1,
        state = #{},
        entry_type = test_entry,
        created_at = erlang:system_time(millisecond),
        metadata = #{}
    },
    {ok, _} = beamai_state_store:save(Store, Entry),

    %% 确认存在
    ?assertEqual(true, beamai_state_store:exists(Store, <<"entry_del">>)),

    %% 删除
    ok = beamai_state_store:delete(Store, <<"entry_del">>),

    %% 确认已删除
    ?assertEqual(false, beamai_state_store:exists(Store, <<"entry_del">>)).

%% @doc 测试存在检查
test_exists(Config) ->
    Store = ?config(store, Config),

    %% 不存在的条目
    ?assertEqual(false, beamai_state_store:exists(Store, <<"nonexistent">>)),

    %% 创建条目
    Entry = #state_entry{
        id = <<"entry_exists">>,
        owner_id = <<"owner_1">>,
        parent_id = undefined,
        branch_id = <<"main">>,
        version = 1,
        state = #{},
        entry_type = test_entry,
        created_at = erlang:system_time(millisecond),
        metadata = #{}
    },
    {ok, _} = beamai_state_store:save(Store, Entry),

    %% 现在存在
    ?assertEqual(true, beamai_state_store:exists(Store, <<"entry_exists">>)).

%% @doc 测试批量操作
test_batch_operations(Config) ->
    Store = ?config(store, Config),
    Now = erlang:system_time(millisecond),

    %% 创建多个条目
    Entries = [
        #state_entry{
            id = <<"batch_1">>,
            owner_id = <<"owner_1">>,
            parent_id = undefined,
            branch_id = <<"main">>,
            version = 1,
            state = #{n => 1},
            entry_type = test_entry,
            created_at = Now,
            metadata = #{}
        },
        #state_entry{
            id = <<"batch_2">>,
            owner_id = <<"owner_1">>,
            parent_id = <<"batch_1">>,
            branch_id = <<"main">>,
            version = 2,
            state = #{n => 2},
            entry_type = test_entry,
            created_at = Now + 1,
            metadata = #{}
        },
        #state_entry{
            id = <<"batch_3">>,
            owner_id = <<"owner_1">>,
            parent_id = <<"batch_2">>,
            branch_id = <<"main">>,
            version = 3,
            state = #{n => 3},
            entry_type = test_entry,
            created_at = Now + 2,
            metadata = #{}
        }
    ],

    %% 批量保存
    {ok, SavedEntries} = beamai_state_store:batch_save(Store, Entries),
    ?assertEqual(3, length(SavedEntries)),

    %% 批量加载
    {ok, LoadedEntries} = beamai_state_store:batch_load(Store, [<<"batch_1">>, <<"batch_2">>, <<"batch_3">>]),
    ?assertEqual(3, length(LoadedEntries)),

    %% 批量删除
    ok = beamai_state_store:batch_delete(Store, [<<"batch_1">>, <<"batch_2">>]),
    ?assertEqual(false, beamai_state_store:exists(Store, <<"batch_1">>)),
    ?assertEqual(false, beamai_state_store:exists(Store, <<"batch_2">>)),
    ?assertEqual(true, beamai_state_store:exists(Store, <<"batch_3">>)).

%% @doc 测试按所有者列出
test_list_by_owner(Config) ->
    Store = ?config(store, Config),
    Now = erlang:system_time(millisecond),

    %% 为不同所有者创建条目
    Entries = [
        #state_entry{
            id = <<"owner1_entry1">>,
            owner_id = <<"owner_1">>,
            parent_id = undefined,
            branch_id = <<"main">>,
            version = 1,
            state = #{},
            entry_type = test_entry,
            created_at = Now,
            metadata = #{}
        },
        #state_entry{
            id = <<"owner1_entry2">>,
            owner_id = <<"owner_1">>,
            parent_id = <<"owner1_entry1">>,
            branch_id = <<"main">>,
            version = 2,
            state = #{},
            entry_type = test_entry,
            created_at = Now + 1,
            metadata = #{}
        },
        #state_entry{
            id = <<"owner2_entry1">>,
            owner_id = <<"owner_2">>,
            parent_id = undefined,
            branch_id = <<"main">>,
            version = 1,
            state = #{},
            entry_type = test_entry,
            created_at = Now + 2,
            metadata = #{}
        }
    ],
    {ok, _} = beamai_state_store:batch_save(Store, Entries),

    %% 按 owner_1 列出
    {ok, Owner1Entries} = beamai_state_store:list_by_owner(Store, <<"owner_1">>),
    ?assertEqual(2, length(Owner1Entries)),

    %% 按 owner_2 列出
    {ok, Owner2Entries} = beamai_state_store:list_by_owner(Store, <<"owner_2">>),
    ?assertEqual(1, length(Owner2Entries)).

%% @doc 测试按分支列出
test_list_by_branch(Config) ->
    Store = ?config(store, Config),
    Now = erlang:system_time(millisecond),

    %% 为不同分支创建条目
    Entries = [
        #state_entry{
            id = <<"main_entry1">>,
            owner_id = <<"owner_1">>,
            parent_id = undefined,
            branch_id = <<"main">>,
            version = 1,
            state = #{},
            entry_type = test_entry,
            created_at = Now,
            metadata = #{}
        },
        #state_entry{
            id = <<"branch_entry1">>,
            owner_id = <<"owner_1">>,
            parent_id = <<"main_entry1">>,
            branch_id = <<"feature">>,
            version = 2,
            state = #{},
            entry_type = test_entry,
            created_at = Now + 1,
            metadata = #{}
        }
    ],
    {ok, _} = beamai_state_store:batch_save(Store, Entries),

    %% 按 main 分支列出
    {ok, MainEntries} = beamai_state_store:list_by_branch(Store, <<"owner_1">>, <<"main">>),
    ?assertEqual(1, length(MainEntries)),

    %% 按 feature 分支列出
    {ok, FeatureEntries} = beamai_state_store:list_by_branch(Store, <<"owner_1">>, <<"feature">>),
    ?assertEqual(1, length(FeatureEntries)).

%% @doc 测试计数
test_count(Config) ->
    Store = ?config(store, Config),
    Now = erlang:system_time(millisecond),

    %% 创建条目
    Entries = [
        #state_entry{
            id = <<"count_1">>,
            owner_id = <<"owner_1">>,
            parent_id = undefined,
            branch_id = <<"main">>,
            version = 1,
            state = #{},
            entry_type = test_entry,
            created_at = Now,
            metadata = #{}
        },
        #state_entry{
            id = <<"count_2">>,
            owner_id = <<"owner_1">>,
            parent_id = <<"count_1">>,
            branch_id = <<"main">>,
            version = 2,
            state = #{},
            entry_type = test_entry,
            created_at = Now + 1,
            metadata = #{}
        },
        #state_entry{
            id = <<"count_3">>,
            owner_id = <<"owner_2">>,
            parent_id = undefined,
            branch_id = <<"main">>,
            version = 1,
            state = #{},
            entry_type = test_entry,
            created_at = Now + 2,
            metadata = #{}
        }
    ],
    {ok, _} = beamai_state_store:batch_save(Store, Entries),

    %% 按所有者计数
    {ok, Count1} = beamai_state_store:count_by_owner(Store, <<"owner_1">>),
    ?assertEqual(2, Count1),

    {ok, Count2} = beamai_state_store:count_by_owner(Store, <<"owner_2">>),
    ?assertEqual(1, Count2).

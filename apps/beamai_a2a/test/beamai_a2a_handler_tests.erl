%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_handler 单元测试（JSON-RPC 方法分发器）
%%%
%%% 此前零直接覆盖。dispatch/3 按方法名路由到各 handler；各 handler 收
%%% Context map（含 id/tasks/contexts），主体是参数校验 + 任务查找 + 错误响应。
%%%
%%% 这里覆盖**不依赖 agent/task/push 进程**的路径：方法路由、缺参、任务未找到。
%%% 成功路径（message/send 建 agent、真实 task pid）需 LLM/进程基础设施，
%%% 由 input_required / http_handler 等集成测试承担。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CTX, #{id => <<"req-1">>, tasks => #{}, contexts => #{}}).

%%====================================================================
%% dispatch 路由
%%====================================================================

%% 未知方法 → -32601 method not found，且回显方法名
unknown_method_test() ->
    {ok, Resp, _} = beamai_a2a_handler:dispatch(<<"no/such/method">>, #{}, ?CTX),
    ?assertEqual(-32601, err_code(Resp)),
    ?assertEqual(<<"no/such/method">>,
                 maps:get(<<"method">>, err_data(Resp))).

%% 响应保留请求 id 与 jsonrpc 版本
response_echoes_id_test() ->
    {ok, Resp, _} = beamai_a2a_handler:dispatch(<<"bad">>, #{}, ?CTX),
    ?assertEqual(<<"req-1">>, maps:get(<<"id">>, Resp)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Resp)).

%%====================================================================
%% 缺参（-32602）—— 所有需要 taskId 的方法
%%====================================================================

missing_task_id_test_() ->
    Methods = [<<"tasks/get">>, <<"tasks/cancel">>,
               <<"tasks/pushNotificationConfig/set">>,
               <<"tasks/pushNotificationConfig/get">>,
               <<"tasks/pushNotificationConfig/delete">>],
    [{binary_to_list(M) ++ " 缺 taskId → -32602",
      fun() ->
          {ok, Resp, _} = beamai_a2a_handler:dispatch(M, #{}, ?CTX),
          ?assertEqual(-32602, err_code(Resp))
      end}
     || M <- Methods].

%%====================================================================
%% 任务未找到（-32001）
%%====================================================================

tasks_get_unknown_task_test() ->
    {ok, Resp, _} = beamai_a2a_handler:dispatch(
                      <<"tasks/get">>, #{<<"taskId">> => <<"nope">>}, ?CTX),
    ?assertEqual(-32001, err_code(Resp)),
    ?assertEqual(<<"nope">>, maps:get(<<"taskId">>, err_data(Resp))).

tasks_cancel_unknown_task_test() ->
    {ok, Resp, _} = beamai_a2a_handler:dispatch(
                      <<"tasks/cancel">>, #{<<"taskId">> => <<"nope">>}, ?CTX),
    ?assertEqual(-32001, err_code(Resp)).

push_set_unknown_task_test() ->
    {ok, Resp, _} = beamai_a2a_handler:dispatch(
                      <<"tasks/pushNotificationConfig/set">>,
                      #{<<"taskId">> => <<"nope">>,
                        <<"pushNotificationConfig">> => #{<<"url">> => <<"http://x">>}},
                      ?CTX),
    ?assertEqual(-32001, err_code(Resp)).

%%====================================================================
%% Context 透传（错误路径不改 Context）
%%====================================================================

context_passthrough_on_error_test() ->
    Ctx = ?CTX,
    {ok, _Resp, NewCtx} = beamai_a2a_handler:dispatch(<<"bad">>, #{}, Ctx),
    ?assertEqual(Ctx, NewCtx).

%%====================================================================
%% 辅助
%%====================================================================

err_code(Resp) ->
    maps:get(<<"code">>, maps:get(<<"error">>, Resp)).

err_data(Resp) ->
    maps:get(<<"data">>, maps:get(<<"error">>, Resp), #{}).

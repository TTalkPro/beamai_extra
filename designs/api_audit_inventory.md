beamai_extra unused exports，按 app 分组并附证据分类。

生成方式：`escript scripts/xref_unused_exports.escript`。
分类只是证据归集，**不代表可以删**：no_evidence 一列是候选，
删除必须人工复核后独立提交。

总计 362 条：internal_only 40 / no_evidence 9 / otp_callback 3 / public_api 215 / test_only 95

## beamai_a2a（124 条）

| module:function/arity | 分类 | 证据 |
|---|---|---|
| `beamai_a2a_auth:add_key/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_auth:add_key/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_auth:authenticate/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_auth:get_config/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_auth:get_key/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_auth:list_keys/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_auth:remove_key/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_auth:set_config/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_auth:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_auth:start_link/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_auth:stats/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_auth:stop/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_auth:update_key/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_auth:validate_key/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card:to_json/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card:to_map/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_card:tools_to_skills/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_card:validate/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_card_cache:clear/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card_cache:exists/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_card_cache:get/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card_cache:get/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card_cache:invalidate/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card_cache:list_keys/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_card_cache:lookup/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_card_cache:put/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card_cache:put/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_card_cache:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_card_cache:start_link/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_card_cache:stats/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_card_cache:stop/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_client:cancel_task/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_client:cancel_task/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_client:create_text_message/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_client:create_text_message/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_client:discover/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_client:get_agent_card/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_client:get_agent_card/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_client:get_task/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_client:get_task/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_client:send_message/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_client:send_message/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_client:send_message/4` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_client:send_message_stream/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_client:send_message_stream/4` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_convert:artifact_to_json/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_convert:build_status_json/2` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_convert:history_entry_to_json/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_convert:message_to_json/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_convert:normalize_part/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_convert:part_to_json/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_cowboy_handler:init/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_handler:handle_message_send/3` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_handler:handle_push_config_delete/3` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_handler:handle_push_config_get/3` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_handler:handle_push_config_set/3` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_handler:handle_tasks_cancel/3` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_handler:handle_tasks_get/3` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_http_handler:build_rate_limit_headers/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:authentication_required/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:decode_request/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:encode_error/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:encode_error/4` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:encode_notification/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:encode_response/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:invalid_params/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:invalid_state_transition/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:is_batch/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:is_error/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:is_notification/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:is_request/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:is_response/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:method_not_found/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:task_already_completed/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_jsonrpc:task_not_found/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_middleware:check_rate_limit_only/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_middleware:get_header/2` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_middleware:get_rate_limit_key/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_middleware:get_rate_limit_key_from_headers/1` | internal_only | 仅本模块内部调用 |
| `beamai_a2a_push:list_webhooks/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_push:list_webhooks/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_push:notify/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_push:register/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_push:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_push:start_link/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_push:stats/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_push:stop/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_rate_limit:check/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_rate_limit:check/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_rate_limit:check_and_consume/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_rate_limit:get_config/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_rate_limit:get_rule/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_rate_limit:list_rules/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_rate_limit:remove_rule/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_rate_limit:reset/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_rate_limit:reset_all/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_rate_limit:set_config/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_rate_limit:set_rule/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_rate_limit:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_rate_limit:start_link/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_rate_limit:stats/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_rate_limit:stop/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_server:complete_task/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_server:fail_task/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_server:handle_json/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_server:handle_json_with_auth/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_server:handle_request/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_server:request_input/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_server:start/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_task:create/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_task:start/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_task:stop/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_task_sup:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_a2a_task_sup:start_task/1` | no_evidence |  |
| `beamai_a2a_types:binary_to_part_kind/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_types:binary_to_task_state/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_types:is_terminal_state/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_types:part_kind_to_binary/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_types:push_event_to_binary/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_types:role_to_binary/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_a2a_utils:generate_id/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_utils:generate_id/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_a2a_utils:timestamp/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_a2a_utils:timestamp_iso8601/0` | public_api | docs/examples 出现 mod:fun( 调用 |

## beamai_agui（65 条）

| module:function/arity | 分类 | 证据 |
|---|---|---|
| `beamai_agui_a2ui:activity_type/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_a2ui:basic_catalog/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_agui_a2ui:catalog_id/0` | no_evidence |  |
| `beamai_agui_a2ui:event_transform/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_a2ui:event_transform/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_a2ui:input_transform/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_a2ui:render_tool/0` | internal_only | 仅本模块内部调用 |
| `beamai_agui_a2ui:render_tool/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_a2ui:surface_event/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_a2ui:surface_ops/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_a2ui:tool_name/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_a2ui:user_action_message/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_a2ui:with_tool/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_a2ui:with_tool/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_cowboy_handler:init/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_emitter:run_finished/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_emitter:run_finished/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_event:activity_delta/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_agui_event:activity_snapshot/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_agui_event:custom/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_event:new/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_event:raw/1` | no_evidence |  |
| `beamai_agui_event:run_finished/3` | test_only | apps/*/test 只出现同名 fun( 调用，需复核 |
| `beamai_agui_event:run_started/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_event:text_message_chunk/2` | internal_only | 仅本模块内部调用 |
| `beamai_agui_event:text_message_chunk/3` | internal_only | 仅本模块内部调用 |
| `beamai_agui_event:to_json/1` | test_only | apps/*/test 只出现同名 fun( 调用，需复核 |
| `beamai_agui_event:tool_call_chunk/2` | internal_only | 仅本模块内部调用 |
| `beamai_agui_event:tool_call_chunk/4` | internal_only | 仅本模块内部调用 |
| `beamai_agui_http_handler:cors_headers/0` | internal_only | 仅本模块内部调用 |
| `beamai_agui_http_handler:cors_origins/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_http_handler:json_headers/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_http_handler:json_response/2` | internal_only | 仅本模块内部调用 |
| `beamai_agui_http_handler:sse_headers/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_mcp_apps:activity_type/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_mcp_apps:app_tools/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_mcp_apps:event_transform/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_mcp_apps:proxy_allowed_methods/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_mcp_apps:proxy_request/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_agui_mcp_apps:server_id_of/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_mcp_apps:ui_resource/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_plugin:compose/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_agui_raw:translate/2` | internal_only | 仅本模块内部调用 |
| `beamai_agui_registry:register/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_agui_registry:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_registry:unregister/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_runner:run/3` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_session:awaiting/1` | no_evidence |  |
| `beamai_agui_session:start_link/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_session:start_run/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_session:stop_run/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_session_sup:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_sessions:count/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_sessions:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_state:escape/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_state:unescape/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_subagent:delegate_tool/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_agui_subagent:fanout_tool/2` | internal_only | 仅本模块内部调用 |
| `beamai_agui_subagent:with_observer/2` | internal_only | 仅本模块内部调用 |
| `beamai_agui_thread:clear/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_thread:store/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_agui_thread:store_name/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_types:content_to_text/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_agui_types:context/1` | internal_only | 仅本模块内部调用 |
| `beamai_agui_types:resume/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |

## beamai_mcp（100 条）

| module:function/arity | 分类 | 证据 |
|---|---|---|
| `beamai_mcp_adapter:adapt_handler/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_adapter:adapt_handler/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_adapter:extract_content/1` | no_evidence |  |
| `beamai_mcp_adapter:extract_text_from_contents/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_adapter:format_error/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_adapter:mcp_tool_to_beamai_tool/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_adapter:mcp_tools_to_beamai_tools/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_client:call_tool/4` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client:connected/3` | otp_callback | gen_statem 状态函数（由状态机分派） |
| `beamai_mcp_client:connecting/3` | otp_callback | gen_statem 状态函数（由状态机分派） |
| `beamai_mcp_client:disconnected/3` | otp_callback | gen_statem 状态函数（由状态机分派） |
| `beamai_mcp_client:get_capabilities/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:get_prompt/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:get_prompt/4` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:get_state/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:initialize/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:list_prompts/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:list_prompts/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:list_resources/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:list_resources/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:list_tools/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client:list_tools/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client:read_resource/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:subscribe_resource/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client:unsubscribe_resource/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_client_registry:count/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_client_registry:get_client/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client_registry:get_client_info/1` | no_evidence |  |
| `beamai_mcp_client_registry:get_clients_by_tag/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client_registry:is_registered/1` | no_evidence |  |
| `beamai_mcp_client_registry:list_client_pids/0` | no_evidence |  |
| `beamai_mcp_client_registry:list_clients/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client_registry:register/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client_registry:register/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client_registry:start_and_register/2` | internal_only | 仅本模块内部调用 |
| `beamai_mcp_client_registry:start_and_register/3` | internal_only | 仅本模块内部调用 |
| `beamai_mcp_client_registry:start_link/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client_registry:stop/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_client_registry:unregister/1` | test_only | apps/*/test 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_client_sup:start_client/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_client_sup:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_client_sup:stop_client/1` | no_evidence |  |
| `beamai_mcp_cowboy_handler:info/3` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_cowboy_handler:init/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_cowboy_handler:terminate/3` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_handler:close/1` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_handler:handle_post/4` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_handler:handle_sse_init/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_handler:handle_sse_message/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_handler:init/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_mcp_jsonrpc:decode_request/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:encode_error/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:encode_error/4` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:encode_response/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:invalid_params/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:is_batch/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:is_error/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:is_notification/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:is_request/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:is_response/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:method_not_found/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:prompt_not_found/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:resource_not_found/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_jsonrpc:tool_not_found/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_server:handle_notification/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_server:register_prompt/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_server:register_resource/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_server:register_tool/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_server:unregister_prompt/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_server:unregister_resource/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_server:unregister_tool/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_server_sup:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_session_registry:count/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_session_registry:start_link/0` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_transport:close/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_transport:connect/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_transport:recv/3` | test_only | apps/*/test 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_transport:send/3` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_transport_http_gun:request/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_transport_http_gun:request/3` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `beamai_mcp_types:client_capabilities_to_map/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:image_content/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:image_content/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_client_capabilities/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_client_capabilities/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_prompt/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_prompt/4` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_prompt_arg/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_prompt_arg/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_resource/4` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_resource/6` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_resource_template/3` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_server_capabilities/0` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_server_capabilities/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_tool/4` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_types:make_tool/5` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_mcp_types:prompt_arg_to_map/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:resource_content/2` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:resource_template_to_map/1` | test_only | apps/*/test 出现 mod:fun( 调用 |
| `beamai_mcp_types:session_to_map/1` | internal_only | 仅本模块内部调用 |

## beamai_rag（39 条）

| module:function/arity | 分类 | 证据 |
|---|---|---|
| `beamai_embeddings:embed/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_embeddings:embed_all/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_embeddings:euclidean_distance/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_embeddings:new_local/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_embeddings:new_local/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_embeddings:new_openai/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_embeddings:new_openai/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_embeddings:new_provider/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_embeddings:new_provider/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_embeddings:normalize/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:get_document_count/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:get_store/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:index_document/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_rag:index_document/4` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_rag:index_documents/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:index_file/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:new/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_rag:new/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_rag:query/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_rag:query/3` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_rag:retrieve/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:retrieve/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:split_text/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag:split_text/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag_reranker:rerank/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag_splitter:get_chunk_size/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag_splitter:get_overlap/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag_splitter:get_separator/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag_splitter:new/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag_utils:get_opt/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_rag_utils:to_float/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_vector_store:add_document/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_vector_store:clear/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_vector_store:delete_document/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_vector_store:get_all_documents/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_vector_store:get_document/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_vector_store:get_documents_by_ids/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_vector_store:new/1` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_vector_store:search/2` | public_api | 模块头 %%% 文档里有调用示例 |

## beamai_tools（34 条）

| module:function/arity | 分类 | 证据 |
|---|---|---|
| `beamai_middleware_presets:call_limit/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:call_limit/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:default/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:development/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:human_approval/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:human_approval/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:human_in_loop/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:minimal/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:model_fallback/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:model_fallback/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:model_retry/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:model_retry/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:production/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:tool_retry/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_middleware_presets:tool_retry/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_tool_human:interrupt_tools/0` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_tool_security:check_command/2` | internal_only | 仅本模块内部调用 |
| `beamai_tool_security:check_path/2` | internal_only | 仅本模块内部调用 |
| `beamai_tool_security:default_config/0` | test_only | apps/*/test 只出现同名 fun( 调用，需复核 |
| `beamai_tools:available/0` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_tools:define_tool/3` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_tools:define_tool/4` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_tools:define_tool/5` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_tools:load/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `beamai_tools:load_all/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_tools:presets/1` | public_api | docs/examples 出现 mod:fun( 调用 |
| `beamai_tools:with_middleware/2` | public_api | 模块头 %%% 文档里有调用示例 |
| `middleware_human_approval:requires_approval/3` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `middleware_model_retry:calculate_delay/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `middleware_model_retry:is_retryable/2` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `middleware_model_retry:is_retryable/3` | public_api | docs/examples 只出现同名 fun( 调用，需复核 |
| `middleware_tool_retry:calculate_delay/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `middleware_tool_retry:is_retryable/2` | public_api | docs/examples 出现 mod:fun( 调用 |
| `middleware_tool_retry:is_retryable/3` | public_api | docs/examples 出现 mod:fun( 调用 |


证据列给出的是**唯一**命中位置；同名函数可能误命中，复核以调用写法（是否真指向本模块）为准。

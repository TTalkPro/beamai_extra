%%%-------------------------------------------------------------------
%%% @doc 人机交互工具（interrupt tool 描述符）
%%%
%%% 提供两个人机交互（human-in-the-loop）能力：
%%% - ask_human: 向用户提问，获取更多信息或澄清
%%% - confirm_action: 请求用户确认重要操作
%%%
%%% == 本模块不提供 kernel 工具 ==
%%%
%%% 它**不是** beamai_tool_behaviour 模块，没有 tools/0，别往 plugins 里挂。
%%% 这里只导出 interrupt_tools/0，供 beamai_agent:new/1 的 `interrupt_tools' 用。
%%%
%%% 原先那套「handler 阻塞等真人」的实现已删除。
%%%
%%% 注意删除的理由**不是**超时：上游 c8dca82/0f73809 已把 tool/gather/batch
%%% 三层缺省全部翻成 infinity，框架不再替用户决定「多久算太久」。所以今天阻塞
%%% 等真人不会再被杀——**它会永远挂着**，这比被杀更糟：
%%%
%%%   - agent 进程被一个人类的注意力钉死，没有任何东西会来救它；
%%%   - 在 a2a 这种请求/响应场景里，一条 HTTP 请求就这么无限期挂住；
%%%   - 状态全在进程内存里，进程一死答复就没了，重启无法接续。
%%%
%%% 根子上，把人塞进工具调用的同步路径这件事本身就是错的——调超时参数救不回来，
%%% 无论那个值是 30 秒还是 infinity。
%%%
%%% == 正道：interrupt/resume ==
%%%
%%% interrupt tool 不是 tool，而是一个「名字 + schema」：告诉模型可以调，
%%% 但它永远不注册进 kernel、永远不执行。模型一调用，agent 就在**执行之前**
%%% 拦下并暂停，把控制权交回调用方；人答完 resume，答复即工具结果回灌模型。
%%% 等待发生在 agent 之外，不占任何超时预算，也天然是串行的（整个 agent 停着）。
%%%
%%% ```erlang
%%% {ok, Agent} = beamai_agent:new(#{
%%%     llm => LlmConfig,
%%%     interrupt_tools => beamai_tool_human:interrupt_tools(),
%%%     %% interrupt_tools 非空会让上游把 on_env_error 自动翻成 pause，
%%%     %% 于是无关的环境类工具失败也会来打断你。不想要就显式关掉。
%%%     on_env_error => proceed
%%% }),
%%% case beamai_agent:run(Agent, <<"帮我清理这批文件"/utf8>>) of
%%%     {interrupt, Info, Agent1} ->
%%%         %% reason 即被中断 tool_call 的参数 map（问题/待确认动作都在里面）
%%%         Answer = 去问真人(maps:get(reason, Info)),
%%%         beamai_agent:resume(Agent1, <<"reply">>, #{message => Answer});
%%%     {ok, Result, _} -> Result
%%% end.
%%% '''
%%%
%%% resume 的 Decision 只能用 `<<"reply">>'（答复即结果）或 `<<"rejected">>'（拒绝）。
%%% **绝不能用 `<<"approved">>'**：那条路径语义是"执行被中断的工具"，会去 kernel
%%% 里找 ask_human——而 interrupt tool 压根没注册进 kernel，只会得到一个
%%% tool_not_found 喂给模型。
%%%
%%% 跨请求/跨重启接人（答复在后续请求才到）：配 `pause_store' +
%%% 稳定的 `conversation_id'，用同样配置重建 agent 再 resume 即可。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_human).

-export([interrupt_tools/0]).

%%====================================================================
%% Interrupt tool 描述符
%%====================================================================

%% @doc 供 beamai_agent:new/1 的 `interrupt_tools' 使用的描述符列表。
%%
%% 形状不是 beamai_tool:tool_spec()：
%%   - 没有 handler——interrupt tool 永不执行；
%%   - parameters 必须是**原生 JSON Schema**。上游 interrupt_tool_to_spec/1 把它
%%     原样塞进 wire schema（`parameters => maps:get(parameters, Tool, ...)'），
%%     不做任何转换。若照抄 beamai_tool 那套
%%     `#{<<"question">> => #{type => string, required => true}}' 的简写，
%%     发给模型的就是一个畸形 schema。
-spec interrupt_tools() -> [map()].
interrupt_tools() ->
    [
        #{
            name => <<"ask_human">>,
            description => <<"Ask the user a question for more information or clarification. "
                             "Execution pauses until the user answers.">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"question">> => #{
                        type => string,
                        description => <<"Question to ask">>},
                    <<"context">> => #{
                        type => string,
                        description => <<"Context for the question">>},
                    <<"options">> => #{
                        type => array,
                        description => <<"Preset answer options">>,
                        items => #{type => string}}
                },
                required => [<<"question">>]
            }
        },
        #{
            name => <<"confirm_action">>,
            description => <<"Request user confirmation before an important operation. "
                             "Execution pauses until the user confirms or rejects.">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"action">> => #{
                        type => string,
                        description => <<"Action description">>},
                    <<"reason">> => #{
                        type => string,
                        description => <<"Why this action is needed">>},
                    <<"consequences">> => #{
                        type => string,
                        description => <<"Potential consequences">>}
                },
                required => [<<"action">>, <<"reason">>]
            }
        }
    ].

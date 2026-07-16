%%%-------------------------------------------------------------------
%%% @doc Shell 命令执行工具模块
%%%
%%% 提供 shell 命令执行能力：
%%% - shell_execute: 执行 shell 命令，支持超时控制和输出限制
%%%
%%% 该工具标记为危险操作，执行前需要人工审批。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_shell).

-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).
-export([handle_execute/2]).

%% 缺省不设时限：命令跑多久是命令的事，只有用户主动杀才真正结束。
%% LLM 仍可经 timeout 参数为**本次调用**自愿设限（它比我们清楚这条命令该多快），
%% 但那是它的选择，不是我们强加的上限。
-define(DEFAULT_TIMEOUT, infinity).

%%====================================================================
%% 工具行为回调
%%====================================================================

tool_info() ->
    #{description => <<"Shell command execution">>,
      tags => [<<"shell">>, <<"system">>]}.

tools() ->
    [shell_execute_tool()].

%%====================================================================
%% 工具定义
%%====================================================================

shell_execute_tool() ->
    #{
        name => <<"shell_execute">>,
        handler => fun ?MODULE:handle_execute/2,
        description => <<"Execute shell command with timeout and output limits.">>,
        tag => <<"shell">>,
        %% 不设时限：命令跑多久是命令的事，只有用户主动杀才真正结束。
        %% 编译、测试、迁移动辄数分钟，被腰斩且只拿到一个 tool_timeout，
        %% 既丢了输出也丢了现场。
        %%
        %% 上游 c8dca82 之后这已是缺省（不声明即 infinity），这里仍显式写出：
        %% 对一个明确要长跑的工具，「不限时」是**声明**而非默认值捡漏。
        %%
        %% LLM 仍可经 timeout 参数为本次调用自愿设限，由 handler 执行
        %% （见 execute_with_timeout/3）。
        %%
        %% 部署方若在 ToolCallingManager 上配了 tool_timeout/batch_timeout，
        %% 或配了 app env tool_gather_timeout，长命令仍会被那一层掐断——
        %% 本 spec 的 infinity 盖得住 manager 的 tool_timeout（工具声明优先），
        %% 但盖不住批级的 batch_timeout。
        timeout => infinity,
        parameters => #{
            <<"command">> => #{type => string, description => <<"Shell command to execute">>, required => true},
            <<"timeout">> => #{type => integer,
                               description => <<"Optional timeout in milliseconds. "
                                                "Omit to run without a time limit.">>},
            <<"working_dir">> => #{type => string, description => <<"Working directory (optional)">>}
        },
        metadata => #{dangerous => true, requires_approval => true}
    }.

%%====================================================================
%% 处理器函数
%%====================================================================

handle_execute(Args, _Context) ->
    Command = maps:get(<<"command">>, Args),
    Timeout = normalize_timeout(maps:get(<<"timeout">>, Args, ?DEFAULT_TIMEOUT)),
    WorkingDir = maps:get(<<"working_dir">>, Args, undefined),
    case beamai_tool_security:check_command(Command) of
        ok -> do_execute(Command, Timeout, WorkingDir);
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

-define(MAX_OUTPUT_SIZE, 102400).

%% @private 归一化 LLM 传入的 timeout。
%%
%% 不设上限：正整数原样采纳（模型自愿为本次调用设限），其余一律 infinity。
%% 负数/0/非整数都当没给——把它们钳成某个小值反而会凭空造出一个我们没承诺的
%% 死线，不如回落到"不限时"这个明确的缺省。
normalize_timeout(T) when is_integer(T), T > 0 -> T;
normalize_timeout(_) -> ?DEFAULT_TIMEOUT.

do_execute(Command, Timeout, WorkingDir) ->
    CommandStr = binary_to_list(Command),
    Opts = build_exec_opts(WorkingDir),
    try
        Result = execute_with_timeout(CommandStr, Timeout, Opts),
        format_result(Result)
    catch
        error:timeout -> {error, {timeout, Timeout}};
        Class:Reason -> {error, {execution_error, {Class, Reason}}}
    end.

build_exec_opts(undefined) -> [];
build_exec_opts(WorkingDir) -> [{cd, binary_to_list(WorkingDir)}].

%% @private 在子进程里跑命令，到点强杀。
%%
%% 必须 spawn_monitor，不能 spawn_link：link 是双向的，`exit(Pid, kill)' 杀掉
%% 被链进程后，`killed' 会顺着链传回来——调用者没 trap_exit 就跟着一起死。
%% 也就是说旧写法里"超时"会**杀死自己的调用者**，而不是返回 {error,{timeout,_}}。
%% monitor 是单向的：子进程怎么死都只变成一条 DOWN 消息。
%%
%% （上游 680a395 之后 handler 本身跑在受监控子进程里，这个自杀行为会被归一成
%%   该工具的 class=>exit 错误而不再带崩整个 agent——但错误类型仍然是错的，
%%   且直接调用 handler 的路径照样会被杀。）
%%
%% 端口在子进程里打开，子进程即端口属主，被 kill 时端口随之关闭。
execute_with_timeout(Command, Timeout, Opts) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(fun() ->
        Parent ! {Ref, execute_command(Command, Timeout, Opts)}
    end),
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, Pid, Reason} ->
            error({execution_crashed, Reason})
    after Timeout ->
        erlang:demonitor(MRef, [flush]),
        exit(Pid, kill),
        error(timeout)
    end.

execute_command(Command, Timeout, Opts) ->
    PortOpts = [stream, exit_status, use_stdio, stderr_to_stdout, binary] ++ Opts,
    try
        Port = open_port({spawn, Command}, PortOpts),
        collect_output(Port, [], Timeout)
    catch
        error:Reason -> {error, Reason}
    end.

%% 收集用与请求一致的 deadline：以前写死 60s，和调用方要求的 Timeout 对不上——
%% 请求 10s 的命令会在 port 层干等到 60s，请求 120s 的又会在 60s 处被截断。
collect_output(Port, Acc, Timeout) ->
    receive
        {Port, {data, Data}} -> collect_output(Port, [Data | Acc], Timeout);
        {Port, {exit_status, Status}} ->
            Output = iolist_to_binary(lists:reverse(Acc)),
            {ok, Status, Output}
    after Timeout ->
        port_close(Port),
        {error, timeout}
    end.

format_result({ok, 0, Output}) ->
    {ok, #{success => true, exit_code => 0, output => truncate_output(Output)}};
format_result({ok, ExitCode, Output}) ->
    {ok, #{success => false, exit_code => ExitCode, output => truncate_output(Output)}};
format_result({error, Reason}) ->
    {error, {execution_error, Reason}}.

truncate_output(Output) when byte_size(Output) > ?MAX_OUTPUT_SIZE ->
    Truncated = binary:part(Output, 0, ?MAX_OUTPUT_SIZE),
    <<Truncated/binary, "\n... (output truncated)">>;
truncate_output(Output) -> Output.

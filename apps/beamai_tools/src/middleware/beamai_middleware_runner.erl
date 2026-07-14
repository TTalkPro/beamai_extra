%%%-------------------------------------------------------------------
%%% @doc 中间件运行器与 Filter 桥接（around 模型）
%%%
%%% 将中间件链转换为 beamai_filter:filter() 列表，注册到 kernel。
%%% 每个中间件变成一个 filter，其私有上下文 (FCtx) 即中间件状态。
%%%
%%% 转换规则：
%%% - 检测中间件模块导出了哪些 around_* 回调
%%% - 为每个中间件创建一个 filter，hooks map 包含对应的 around_* 键
%%% - filter 的 around_fun 直接委托给中间件回调
%%% - InitFCtx = 中间件 init/1 返回的初始状态
%%%
%%% 注册顺序：按优先级排序（priority 小的 = 外层 = 先执行）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_runner).

-export([init/1, to_filters/1]).

-export_type([middleware_chain/0]).

%%====================================================================
%% 类型定义
%%%-------------------------------------------------------------------

-type middleware_chain() :: [beamai_middleware:middleware()].

%%====================================================================
%% 公共 API
%%%-------------------------------------------------------------------

%% @doc 从配置规格列表初始化中间件链。
%%
%% 支持以下输入格式：
%% - {Module, Opts, Priority}: 指定模块、配置和优先级
%% - {Module, Opts}: 指定模块和配置，默认优先级 100
%% - Module: 仅模块名，空配置，默认优先级 100
%%
%% 返回按优先级升序排列的中间件链（优先级越小越外层）。
-spec init([term()]) -> middleware_chain().
init(MiddlewareSpecs) ->
    Middlewares = lists:map(fun init_single/1, MiddlewareSpecs),
    lists:sort(fun(#{priority := P1}, #{priority := P2}) -> P1 =< P2 end, Middlewares).

%% @doc 将中间件链转换为 beamai_filter:filter() 列表。
%%
%% 每个中间件变成一个 filter。filter 的 hooks map 包含该中间件
%% 导出的所有 around_* 回调。filter 的 InitFCtx = 中间件初始状态。
%%
%% 无任何 around_* 回调的中间件会被跳过（不产生 filter）。
-spec to_filters(middleware_chain()) -> [beamai_filter:filter()].
to_filters(Chain) ->
    lists:flatmap(fun middleware_to_filter/1, Chain).

%%====================================================================
%% 内部函数
%%%-------------------------------------------------------------------

%% @private 初始化单个中间件规格
init_single({Module, Opts, Priority}) when is_atom(Module), is_map(Opts), is_integer(Priority) ->
    State = call_init(Module, Opts),
    #{module => Module, state => State, priority => Priority};
init_single({Module, Opts}) when is_atom(Module), is_map(Opts) ->
    State = call_init(Module, Opts),
    #{module => Module, state => State, priority => 100};
init_single(Module) when is_atom(Module) ->
    State = call_init(Module, #{}),
    #{module => Module, state => State, priority => 100}.

%% @private 调用中间件 init/1（未导出则用 Opts 作状态）
call_init(Module, Opts) ->
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, init, 1) of
        true -> Module:init(Opts);
        false -> Opts
    end.

%% @private 将单个中间件转换为 filter（无 around_* 回调则返回 []）
middleware_to_filter(#{module := Module, state := State}) ->
    Hooks = collect_hooks(Module),
    case map_size(Hooks) of
        0 -> [];
        _ ->
            FilterName = filter_name(Module),
            [beamai_filter:new(FilterName, Hooks, State)]
    end.

%% @private 检测中间件导出了哪些 around_* 回调，构建 hooks map
collect_hooks(Module) ->
    Hooks0 = #{},
    Hooks1 = maybe_add_hook(Module, around_chat, Hooks0),
    Hooks2 = maybe_add_hook(Module, around_tool, Hooks1),
    maybe_add_hook(Module, around_turn, Hooks2).

%% @private 如果模块导出了指定的 around_* 回调，添加到 hooks map
maybe_add_hook(Module, HookName, Hooks) ->
    case erlang:function_exported(Module, HookName, 3) of
        true ->
            Hooks#{HookName => fun(Req, FCtx, Next) ->
                case Module:HookName(Req, FCtx, Next) of
                    {Resp, NewFCtx} -> {Resp, NewFCtx};
                    Resp -> Resp
                end
            end};
        false ->
            Hooks
    end.

%% @private 生成 filter 名称
filter_name(Module) ->
    iolist_to_binary(io_lib:format("mw_~s", [Module])).

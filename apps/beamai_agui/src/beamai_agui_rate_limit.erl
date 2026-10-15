%%%-------------------------------------------------------------------
%%% @doc AG-UI fixed-window request rate limiting.
%%%
%%% Limits are checked per key over both a minute and an hour window. The
%%% service is disabled by default; hosts turn it on explicitly.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_rate_limit).

-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0]).
-export([check_and_consume/1, set_rule/2, remove_rule/1]).
-export([set_config/1, is_enabled/0, reset_all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([result/0]).

-type key() :: binary().
-type result() :: {ok, map()} | {error, rate_limited | rate_limit_disabled, map()}.

-record(state, {config :: map(), stats :: map()}).
-record(rule, {key :: key(), minute :: pos_integer(), hour :: pos_integer()}).
-record(bucket, {key :: key(), minute_start :: non_neg_integer(),
                 minute_count :: non_neg_integer(),
                 hour_start :: non_neg_integer(),
                 hour_count :: non_neg_integer()}).

-define(SERVER, ?MODULE).
-define(RULES, beamai_agui_rate_rules).
-define(BUCKETS, beamai_agui_rate_buckets).
-define(MINUTE, 60_000).
-define(HOUR, 3_600_000).

%%====================================================================
%% API
%%====================================================================

%% @doc Start rate limiting with defaults (disabled).
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start rate limiting with configuration.
%%
%% Defaults are 60 requests per minute and 1000 per hour. `enabled' is false
%% until the host explicitly turns it on.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Stop a standalone rate limiter.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Check and consume one request slot.
%%
%% The returned map carries the active limits and reset time. Rejections also
%% carry `retry_after' in milliseconds.
-spec check_and_consume(key()) -> result().
check_and_consume(Key) when is_binary(Key) ->
    gen_server:call(?SERVER, {consume, Key}).

%% @doc Set per-key limits.
-spec set_rule(key(), map()) -> {ok, map()} | {error, invalid_rule}.
set_rule(Key, Rule) when is_binary(Key), is_map(Rule) ->
    gen_server:call(?SERVER, {set_rule, Key, Rule}).

%% @doc Remove a per-key rule; the key falls back to global defaults.
-spec remove_rule(key()) -> ok.
remove_rule(Key) when is_binary(Key) ->
    gen_server:call(?SERVER, {remove_rule, Key}).

%% @doc Update global limiter configuration.
-spec set_config(map()) -> ok.
set_config(Config) when is_map(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

%% @doc Return whether rate limiting is enabled.
-spec is_enabled() -> boolean().
is_enabled() ->
    gen_server:call(?SERVER, is_enabled).

%% @doc Forget all bucket state without changing rules.
-spec reset_all() -> ok.
reset_all() ->
    gen_server:call(?SERVER, reset_all).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the local rate-limit worker.
-spec init(map()) -> {ok, #state{}}.
init(Config) ->
    ok = ensure_tables(),
    {ok, #state{config = maps:merge(#{enabled => false,
                                      requests_per_minute => 60,
                                      requests_per_hour => 1000}, Config),
                stats = #{allowed => 0, rejected => 0}}}.

%% @doc Handle synchronous limit and configuration calls.
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({consume, Key}, _From, State) ->
    case maps:get(enabled, State#state.config, false) of
        false ->
            {reply, {error, rate_limit_disabled, #{}}, State};
        true ->
            {Result, Counted} = consume(Key, State),
            {reply, Result, State#state{stats = Counted}}
    end;
handle_call({set_rule, Key, Rule}, _From, State) ->
    case normalize_rule(Key, Rule, State) of
        {ok, Record} ->
            true = ets:insert(?RULES, Record),
            {reply, {ok, rule_map(Record)}, State};
        Error ->
            {reply, Error, State}
    end;
handle_call({remove_rule, Key}, _From, State) ->
    true = ets:delete(?RULES, Key),
    true = ets:delete(?BUCKETS, Key),
    {reply, ok, State};
handle_call({set_config, Config}, _From, State) ->
    {reply, ok, State#state{config = maps:merge(State#state.config, Config)}};
handle_call(is_enabled, _From, State) ->
    {reply, maps:get(enabled, State#state.config, false), State};
handle_call(reset_all, _From, State) ->
    true = ets:delete_all_objects(?BUCKETS),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous messages; none are currently defined.
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Message, State) ->
    {noreply, State}.

%% @doc Handle out-of-band worker messages.
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Stop the worker and release its ETS tables.
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Retain state across code upgrades.
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

ensure_tables() ->
    [ensure_table(Name, KeyPos) ||
        {Name, KeyPos} <- [{?RULES, #rule.key}, {?BUCKETS, #bucket.key}]],
    ok.

ensure_table(Name, KeyPos) ->
    case ets:info(Name) of
        undefined ->
            ets:new(Name, [named_table, set, public, {keypos, KeyPos},
                           {read_concurrency, true},
                           {write_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

consume(Key, #state{config = Config} = State) ->
    Rule = rule(Key, Config),
    Now = os:system_time(millisecond),
    Bucket = roll_bucket(now_bucket(Key, Now), Now),
    case limits(Bucket, Rule, Now) of
        {ok, Info, NewBucket} ->
            true = ets:insert(?BUCKETS, NewBucket),
            {{ok, Info}, bump(allowed, State#state.stats)};
        {limited, RetryAfter, Info} ->
            true = ets:insert(?BUCKETS, Bucket),
            Rejection = Info#{retry_after => RetryAfter},
            {{error, rate_limited, Rejection}, bump(rejected, State#state.stats)}
    end.

rule(Key, Config) ->
    case ets:lookup(?RULES, Key) of
        [Record] -> rule_map(Record);
        [] -> #{minute => maps:get(requests_per_minute, Config),
                hour => maps:get(requests_per_hour, Config)}
    end.

now_bucket(Key, Now) ->
    case ets:lookup(?BUCKETS, Key) of
        [Bucket] -> Bucket;
        [] -> #bucket{key = Key, minute_start = Now, minute_count = 0,
                      hour_start = Now, hour_count = 0}
    end.

roll_bucket(Bucket, Now) ->
    Bucket#bucket{
      minute_start = maybe_reset(Bucket#bucket.minute_start, Now, ?MINUTE),
      minute_count = maybe_zero(Bucket#bucket.minute_start, Now, ?MINUTE,
                                Bucket#bucket.minute_count),
      hour_start = maybe_reset(Bucket#bucket.hour_start, Now, ?HOUR),
      hour_count = maybe_zero(Bucket#bucket.hour_start, Now, ?HOUR,
                              Bucket#bucket.hour_count)}.

maybe_reset(Start, Now, Window) ->
    case Now - Start >= Window of
        true -> Now;
        false -> Start
    end.

maybe_zero(Start, Now, Window, Count) ->
    case Now - Start >= Window of
        true -> 0;
        false -> Count
    end.

limits(#bucket{minute_count = MinuteCount, hour_count = HourCount} = Bucket,
       Rule, Now) ->
    MinuteLimit = maps:get(minute, Rule),
    HourLimit = maps:get(hour, Rule),
    if
        MinuteCount >= MinuteLimit ->
            {limited, ?MINUTE - (Now - Bucket#bucket.minute_start),
             #{limit => MinuteLimit, window => <<"minute">>}};
        HourCount >= HourLimit ->
            {limited, ?HOUR - (Now - Bucket#bucket.hour_start),
             #{limit => HourLimit, window => <<"hour">>}};
        true ->
            NewBucket = Bucket#bucket{minute_count = MinuteCount + 1,
                                      hour_count = HourCount + 1},
            Info = #{minute_limit => MinuteLimit,
                     minute_remaining => MinuteLimit - MinuteCount - 1,
                     hour_limit => HourLimit,
                     hour_remaining => HourLimit - HourCount - 1,
                     minute_reset_at => Bucket#bucket.minute_start + ?MINUTE,
                     hour_reset_at => Bucket#bucket.hour_start + ?HOUR},
            {ok, Info, NewBucket}
    end.

normalize_rule(Key, Rule, #state{config = Config}) ->
    try
        {ok, #rule{key = Key,
                   minute = positive(maps:get(requests_per_minute, Rule,
                                              maps:get(requests_per_minute, Config))),
                   hour = positive(maps:get(requests_per_hour, Rule,
                                            maps:get(requests_per_hour, Config)))}}
    catch
        error:badarg -> {error, invalid_rule}
    end.

positive(Value) when is_integer(Value), Value > 0 ->
    Value;
positive(_Value) ->
    erlang:error(badarg).

rule_map(#rule{key = Key, minute = Minute, hour = Hour}) ->
    #{key => Key, requests_per_minute => Minute, requests_per_hour => Hour}.

bump(Field, Stats) ->
    maps:update_with(Field, fun(V) -> V + 1 end, 1, Stats).

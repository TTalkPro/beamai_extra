%%%-------------------------------------------------------------------
%%% @doc AG-UI API key authentication.
%%%
%%% The service is disabled until the host enables it, so existing endpoints keep
%%% their current behaviour. When enabled, keys can be supplied as
%%% `Authorization: Bearer <key>' or `X-API-Key: <key>'.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_auth).

-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0]).
-export([authenticate/1, validate_key/1, add_key/2, remove_key/1]).
-export([set_config/1, is_enabled/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([headers/0, key_info/0, result/0]).

-type headers() :: [{binary(), binary()}].
-type permissions() :: all | [atom()].
-type key_info() :: #{
    key := binary(),
    name := binary(),
    permissions := permissions(),
    expires_at := non_neg_integer() | undefined,
    metadata := map()
}.
-type result() :: {ok, key_info()} |
                  {error, auth_disabled | missing_api_key | invalid_api_key |
                          key_expired}.

-record(state, {config :: map(), stats :: map()}).
-record(key, {key :: binary(), name :: binary(), permissions :: permissions(),
              expires_at :: non_neg_integer() | undefined, metadata :: map()}).

-define(SERVER, ?MODULE).
-define(TABLE, beamai_agui_auth_keys).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the authentication service with default (disabled) configuration.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the authentication service.
%%
%% `enabled' defaults to `false'; host configuration should turn it on
%% deliberately so an upgrade never locks out an existing frontend.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Stop a standalone authentication service.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Authenticate request headers.
-spec authenticate(headers()) -> result().
authenticate(Headers) when is_list(Headers) ->
    gen_server:call(?SERVER, {authenticate, Headers}).

%% @doc Validate one API key without looking at HTTP headers.
-spec validate_key(binary()) -> result().
validate_key(Key) when is_binary(Key) ->
    gen_server:call(?SERVER, {validate_key, Key}).

%% @doc Add or replace an API key.
%%
%% Metadata accepts `name', `permissions' and `expires_at' (milliseconds since
%% epoch). Additional fields are retained as key metadata.
-spec add_key(binary(), map()) -> {ok, key_info()} | {error, invalid_api_key}.
add_key(Key, Metadata) when is_binary(Key), is_map(Metadata) ->
    gen_server:call(?SERVER, {add_key, Key, Metadata}).

%% @doc Remove an API key.
-spec remove_key(binary()) -> ok.
remove_key(Key) when is_binary(Key) ->
    gen_server:call(?SERVER, {remove_key, Key}).

%% @doc Update service configuration.
-spec set_config(map()) -> ok.
set_config(Config) when is_map(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

%% @doc Return whether authentication is enabled.
-spec is_enabled() -> boolean().
is_enabled() ->
    gen_server:call(?SERVER, is_enabled).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the local authentication worker.
-spec init(map()) -> {ok, #state{}}.
init(Config) ->
    ok = ensure_table(),
    {ok, #state{config = maps:merge(#{enabled => false}, Config),
                stats = #{success => 0, failed => 0}}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
%% @doc Handle synchronous authentication and key management calls.
handle_call({authenticate, Headers}, _From, State) ->
    case do_authenticate(Headers, State#state.config) of
        {ok, Info} ->
            {reply, {ok, Info}, bump(success, State)};
        {error, Reason} ->
            {reply, {error, Reason}, bump(failed, State)}
    end;
handle_call({validate_key, Key}, _From, State) ->
    case lookup_key(Key) of
        {ok, Info} -> {reply, {ok, Info}, bump(success, State)};
        Error -> {reply, Error, bump(failed, State)}
    end;
handle_call({add_key, Key, Metadata}, _From, State) ->
    case normalize_key(Key, Metadata) of
        {ok, Record} ->
            true = ets:insert(?TABLE, Record),
            {reply, {ok, key_info(Record)}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({remove_key, Key}, _From, State) ->
    true = ets:delete(?TABLE, Key),
    {reply, ok, State};
handle_call({set_config, Config}, _From, State) ->
    {reply, ok, State#state{config = maps:merge(State#state.config, Config)}};
handle_call(is_enabled, _From, State) ->
    {reply, maps:get(enabled, State#state.config, false), State};
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

%% @doc Stop the worker and release its ETS table.
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

ensure_table() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, set, public,
                             {keypos, #key.key},
                             {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

do_authenticate(Headers, Config) ->
    case maps:get(enabled, Config, false) of
        false ->
            {error, auth_disabled};
        true ->
            case api_key(Headers) of
                undefined -> {error, missing_api_key};
                Key -> lookup_key(Key)
            end
    end.

api_key(Headers) ->
    case header(Headers, <<"x-api-key">>) of
        undefined -> authorization_key(header(Headers, <<"authorization">>));
        Key -> Key
    end.

authorization_key(undefined) ->
    undefined;
authorization_key(Value) when is_binary(Value) ->
    case binary:split(Value, <<" ">>, [trim_all]) of
        [Scheme, Key] ->
            case string:lowercase(Scheme) of
                <<"bearer">> -> Key;
                <<"apikey">> -> Key;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

lookup_key(Key) ->
    case ets:lookup(?TABLE, Key) of
        [Record] -> validate_expiry(Record);
        [] -> {error, invalid_api_key}
    end.

validate_expiry(#key{expires_at = undefined} = Record) ->
    {ok, key_info(Record)};
validate_expiry(#key{expires_at = ExpiresAt} = Record) ->
    case os:system_time(millisecond) < ExpiresAt of
        true -> {ok, key_info(Record)};
        false -> {error, key_expired}
    end.

normalize_key(Key, Metadata) when is_binary(Key), Key =/= <<>> ->
    {ok, #key{key = Key,
              name = maps:get(name, Metadata, <<>>),
              permissions = maps:get(permissions, Metadata, all),
              expires_at = maps:get(expires_at, Metadata, undefined),
              metadata = maps:without([name, permissions, expires_at], Metadata)}};
normalize_key(_Key, _Metadata) ->
    {error, invalid_api_key}.

key_info(#key{key = Key, name = Name, permissions = Permissions,
              expires_at = ExpiresAt, metadata = Metadata}) ->
    #{key => Key, name => Name, permissions => Permissions,
      expires_at => ExpiresAt, metadata => Metadata}.

bump(Field, #state{stats = Stats} = State) ->
    State#state{stats = maps:update_with(Field, fun(V) -> V + 1 end, 1, Stats)}.

header([], _Name) ->
    undefined;
header([{Name, Value} | _Rest], Name) when is_binary(Name), is_binary(Value) ->
    Value;
header([{Name0, Value} | Rest], Name) when is_binary(Name0), is_binary(Value) ->
    case string:lowercase(Name0) =:= Name of
        true -> Value;
        false -> header(Rest, Name)
    end;
header([_ | Rest], Name) ->
    header(Rest, Name).

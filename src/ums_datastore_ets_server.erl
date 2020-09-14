%%-------------------------------------------------------------------
%% @doc ums ETS data store server
%% @end
%%-------------------------------------------------------------------

-module(ums_datastore_ets_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/0]).
-export([init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/1]).
-export([drop/1]).
-export([get/2]).
-export([list/1]).
-export([put/3]).
-export([delete/2]).

%% Types
-type state() :: #{
    tids := tids()
}.

-type tids() :: #{
    atom() => ets:tab()
}.

%%-------------------------------------------------------------------
%% Supervision
%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{tids => #{}}}.

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

%% @doc Create a table
-spec create(Table :: atom()) -> ok | {error, already_exists, string()}.
create(Table) ->
    gen_server:call(?MODULE, {create, Table}).

%% @doc Drop a table
-spec drop(Table :: atom()) -> ok | {error, table_not_found, string()}.
drop(Table) ->
    gen_server:call(?MODULE, {drop, Table}).

%% @doc Get an item from table by its key
-spec get(Table :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.
get(Table, Key) ->
    gen_server:call(?MODULE, {get, Table, Key}).

%% @doc Get all items in given table
-spec list(Table :: atom()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.
list(Table) ->
    gen_server:call(?MODULE, {list, Table}).

%% @doc Store an item by its key in given table
-spec put(Table :: atom(), Key :: binary(), Value :: term()) ->
    ok |
    {error, table_not_found, string()}.
put(Table, Key, Value) ->
    gen_server:call(?MODULE, {put, Table, Key, Value}).

%% @doc Delete an item by its key
-spec delete(Table :: atom(), Key :: binary()) ->
    ok |
    {error, table_not_found, string()}.
delete(Table, Key) ->
    gen_server:call(?MODULE, {delete, Table, Key}).

%%-------------------------------------------------------------------
%% Behavior callbacks
%%-------------------------------------------------------------------

-type from() :: {pid(), term()}.
-spec handle_call(term(), from(), state()) -> {reply, term(), state()}.
handle_call({create, Table}, _From, #{tids := Tids} = State) ->
    {Reply, NewTids} = create_table(table_exists(Table, Tids), Table, Tids),
    NewState = State#{tids := NewTids},
    {reply, Reply, NewState};
handle_call({drop, Table}, _From, #{tids := Tids} = State) ->
    {Reply, NewTids} = drop_table(table_exists(Table, Tids), Table, Tids),
    NewState = State#{tids := NewTids},
    {reply, Reply, NewState};
handle_call({get, Table, Key}, _From, #{tids := Tids} = State) ->
    {reply, get_by_key(table_exists(Table, Tids), Key, Table, Tids), State};
handle_call({list, Table}, _From, #{tids := Tids} = State) ->
    {reply, get_all(table_exists(Table, Tids), Table, Tids), State};
handle_call({put, Table, Key, Value}, _From, #{tids := Tids} = State) ->
    {reply, put(table_exists(Table, Tids), Key, Value, Table, Tids), State};
handle_call({delete, Table, Key}, _From, #{tids := Tids} = State) ->
    {reply, delete(table_exists(Table, Tids), Key, Table, Tids), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%-------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------

-spec table_exists(Table :: atom(), Tids :: tids()) -> boolean().
table_exists(Table, Tids) ->
    maps:is_key(Table, Tids).

-spec create_table(TableExists :: boolean(), Table :: atom(), Tids :: tids()) ->
    {ok, NewTids :: tids()} |
    {{error, already_exists, string()}, Tids :: tids()}.
create_table(true, Table, Tids) ->
    {{error, already_exists, "ETS table " ++ atom_to_list(Table) ++ " already exists."}, Tids};
create_table(false, Table, Tids) ->
    Tid = ets:new(Table, [set, {read_concurrency, true}]),
    {ok, Tids#{Table => Tid}}.

-spec drop_table(TableExists :: boolean(), Table :: atom(), Tids :: tids()) ->
    {ok, NewTids :: tids()} |
    {{error, table_not_found, string()}, Tids :: tids()}.
drop_table(true, Table, Tids) ->
    Tid = maps:get(Table, Tids),
    true = ets:delete(Tid),
    NewTids = maps:remove(Table, Tids),
    {ok, NewTids};
drop_table(false, Table, Tids) ->
    {{error, table_not_found, "ETS table " ++ atom_to_list(Table) ++ " not found."}, Tids}.

-spec get_by_key(TableExists :: boolean(), Key :: binary(), Table :: atom(), Tids :: tids()) ->
    [{binary(), term()}] |
    {error, table_not_found, string()}.
get_by_key(true, Key, Table, Tids) ->
    Tid = maps:get(Table, Tids),
    ets:lookup(Tid, Key);
get_by_key(false, _Key, Table, _Tids) ->
    {error, table_not_found, "ETS table " ++ atom_to_list(Table) ++ " not found."}.

-spec get_all(TableExists :: boolean(), Table :: atom(), Tids :: tids()) ->
    [{binary(), term()}] |
    {error, table_not_found, string()}.
get_all(true, Table, Tids) ->
    Tid = maps:get(Table, Tids),
    ets:tab2list(Tid);
get_all(false, Table, _Tids) ->
    {error, table_not_found, "ETS table " ++ atom_to_list(Table) ++ " not found."}.

-spec put(TableExists :: boolean(), Key :: binary(), Value :: term(), Table :: atom(), Tids :: tids()) ->
    ok |
    {error, table_not_found, string()}.
put(true, Key, Value, Table, Tids) ->
    Tid = maps:get(Table, Tids),
    true = ets:insert(Tid, {Key, Value}),
    ok;
put(false, _Key, _Value, Table, _Tids) ->
    {error, table_not_found, "ETS table " ++ atom_to_list(Table) ++ " not found."}.

-spec delete(TableExists :: boolean(), Key :: binary(), Table :: atom(), Tids :: tids()) ->
    ok |
    {error, table_not_found, string()}.
delete(true, Key, Table, Tids) ->
    Tid = maps:get(Table, Tids),
    true = ets:delete(Tid, Key),
    ok;
delete(false, _Key, Table, _Tids) ->
    {error, table_not_found, "ETS table " ++ atom_to_list(Table) ++ " not found."}.

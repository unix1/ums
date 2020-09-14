%%-------------------------------------------------------------------
%% @doc ums ETS data store component
%% @end
%%-------------------------------------------------------------------

-module(ums_datastore_ets).

-behaviour(ums_datastore).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% Behavior callbacks
-export([init/1]).
-export([create/1]).
-export([drop/1]).
-export([get/2]).
-export([list/1]).
-export([put/3]).
-export([delete/2]).
-export([terminate/0]).

%%-------------------------------------------------------------------
%% Behavior callbacks
%%-------------------------------------------------------------------

-spec init(SupRef :: supervisor:sup_ref()) -> ok.
init(SupRef) ->
    EtsSup = ?CHILD(ums_datastore_ets_sup, ums_datastore_ets_sup, [], supervisor),
    {ok, _Pid} = supervisor:start_child(SupRef, EtsSup),
    ok.

-spec create(Table :: atom()) -> ok | {error, already_exists, string()}.
create(Table) ->
    ums_datastore_ets_server:create(Table).

-spec drop(Table :: atom()) -> ok | {error, table_not_found, string()}.
drop(Table) ->
    ums_datastore_ets_server:drop(Table).

-spec get(Table :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.
get(Table, Key) ->
    ums_datastore_ets_server:get(Table, Key).

-spec list(Table :: atom()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.
list(Table) ->
    ums_datastore_ets_server:list(Table).

-spec put(Table :: atom(), Key :: binary(), Value :: term()) -> ok | {error, table_not_found, string()}.
put(Table, Key, Value) ->
    ums_datastore_ets_server:put(Table, Key, Value).

-spec delete(Table :: atom(), Key :: binary()) -> ok | {error, table_not_found, string()}.
delete(Table, Key) ->
    ums_datastore_ets_server:delete(Table, Key).

-spec terminate() -> ok.
terminate() -> ok.

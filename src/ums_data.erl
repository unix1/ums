%%-------------------------------------------------------------------
%% @doc ums data
%% @end
%%-------------------------------------------------------------------

-module(ums_data).

%% API
-export([init/0]).
-export([create/2]).
-export([ensure_created/2]).
-export([drop/2]).
-export([put/4]).
-export([get/3]).
-export([list/2]).
-export([delete/3]).

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

-spec init() -> ok.
init() ->
    ok = ums_data_org:ensure_created().

-spec create(Storage :: atom(), Table :: atom()) ->
    ok |
    {error, already_exists, string()}.
create(Storage, Table) ->
    Storage:create(Table).

-spec ensure_created(Storage :: atom(), Table :: atom()) -> ok.
ensure_created(Storage, Table) ->
    ok = case create(Storage, Table) of
        ok -> ok;
        {error, already_exists, _} -> ok
    end.

-spec drop(Storage :: atom(), Table :: atom()) ->
    ok |
    {error, table_not_found, string()}.
drop(Storage, Table) ->
    Storage:drop(Table).

-spec put(Storage :: atom(), Table :: atom(), Key :: binary(), Value :: term()) ->
    ok |
    {error, table_not_found, string()}.
put(Storage, Table, Key, Value) ->
    Storage:put(Table, Key, Value).

-spec get(Storage :: atom(), Table :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.
get(Storage, Table, Key) ->
    Storage:get(Table, Key).

-spec list(Storage :: atom(), Table :: atom()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.
list(Storage, Table) ->
    Storage:list(Table).

-spec delete(Storage :: atom(), Table :: atom(), Key :: binary()) ->
    ok |
    {error, table_not_found, string()}.
delete(Storage, Table, Key) ->
    Storage:delete(Table, Key).

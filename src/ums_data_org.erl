%%-------------------------------------------------------------------
%% @doc ums organization data
%% @end
%%-------------------------------------------------------------------

-module(ums_data_org).

%% API
-export([create/0]).
-export([ensure_created/0]).
-export([drop/0]).
-export([put/2]).
-export([get/1]).
-export([list/0]).
-export([delete/1]).

-define(TABLE, org).

-type attrs() :: #{
    key := binary(),
    name := binary()
}.

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

-spec create() ->
    ok |
    {error, already_exists, string()}.
create() ->
    ums_data:create(storage(), ?TABLE).

-spec ensure_created() -> ok.
ensure_created() ->
    ums_data:ensure_created(storage(), ?TABLE).

-spec drop() ->
    ok |
    {error, table_not_found, string()}.
drop() ->
    ums_data:drop(storage(), ?TABLE).

-spec put(Key :: binary(), Attrs :: attrs()) ->
    ok |
    {error, table_not_found, string()}.
put(Key, Attrs) ->
    ums_data:put(storage(), ?TABLE, Key, Attrs).

-spec get(Key :: binary()) ->
    [{Key :: binary(), Attrs :: attrs()}] |
    {error, table_not_found, string()}.
get(Key) ->
    ums_data:get(storage(), ?TABLE, Key).

-spec list() ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.
list() ->
    ums_data:list(storage(), ?TABLE).

-spec delete(Key :: binary()) ->
    ok |
    {error, table_not_found, string()}.
delete(Key) ->
    ums_data:delete(storage(), ?TABLE, Key).

%%-------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------

storage() ->
    ums_settings:get(storage_module).

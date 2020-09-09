%%%-------------------------------------------------------------------
%% @doc ums data store behavior
%% @end
%%%-------------------------------------------------------------------

-module(ums_datastore).

%% Initialize storage resources
-callback init(Sup :: atom()) -> ok.

%% Create table with the given name.
-callback create(Table :: atom()) ->
    ok |
    {error, already_exists, string()}.

%% Drop table with the given name.
-callback drop(Table :: atom()) ->
    ok |
    {error, table_not_found, string()}.

%% Store given key and value in given table.
-callback put(atom(), binary(), term()) ->
    ok |
    {error, table_not_found, string()}.

%% Get value for given key from given table.
-callback get(Table :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.

%% List all keys and values from given table.
-callback list(Table :: atom()) ->
    [{Key :: binary(), Value :: term()}] |
    {error, table_not_found, string()}.

%% Delete key value pair for given key.
-callback delete(Table :: atom(), Key :: binary()) ->
    ok |
    {error, table_not_found, string()}.

%% Clean up storage resources during shutdown
-callback terminate() -> ok.

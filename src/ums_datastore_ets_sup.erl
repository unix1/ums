%%-------------------------------------------------------------------
%% @doc ums ETS data store supervisor
%% @end
%%-------------------------------------------------------------------

-module(ums_datastore_ets_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%-------------------------------------------------------------------
%% Supervision
%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 0, 1}, children()}}.

%%-------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------

-spec children() -> [supervisor:child_spec()].
children() ->
    EtsStorageServer = ?CHILD(ums_datastore_ets_server, ums_datastore_ets_server, [], worker),
    [EtsStorageServer].

%%-------------------------------------------------------------------
%% @doc ums top level supervisor.
%% @end
%%-------------------------------------------------------------------

-module(ums_sup).

-behaviour(supervisor).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% Supervision
-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%-------------------------------------------------------------------
%% Supervision
%%-------------------------------------------------------------------

start_link(StorageSup) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [StorageSup]).

init([StorageSup]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = children(StorageSup),
    {ok, {SupFlags, ChildSpecs}}.

%%-------------------------------------------------------------------
%% Internal
%%-------------------------------------------------------------------

-spec children(StorageSup :: atom()) -> [supervisor:child_spec()].
children(StorageSup) ->
    StorageSupSpec = ?CHILD(StorageSup, StorageSup, [], supervisor),
    [StorageSupSpec].

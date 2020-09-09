%%-------------------------------------------------------------------
%% @doc ums public API
%% @end
%%-------------------------------------------------------------------

-module(ums_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", ums_top_h, []},
            {"/[:org_key]", ums_list_h, []},
            {"/[:org_key]/[:proj_key]", ums_item_h, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    ums_sup:start_link(ums_settings:get(storage_supervisor)).

stop(_State) ->
    ok = cowboy:stop_listener(http),
    ok.

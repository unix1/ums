%%-------------------------------------------------------------------
%% @doc ums organization data
%% @end
%%-------------------------------------------------------------------

-module(ums_data_org).

-export([put/2]).

-define(TABLE, org).

-type attrs() :: #{
    key := binary(),
    name := binary()
}.

-spec put(binary(), attrs()) -> ok.
put(Id, Attrs) ->
    ums_data:put(?TABLE, Id, Attrs).

-module(ums_general_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([nothing/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        nothing
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ums),
    Config.

end_per_suite(_) ->
    ok = application:stop(ums),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

nothing(_) ->
    ok.

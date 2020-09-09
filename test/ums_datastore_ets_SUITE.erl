-module(ums_datastore_ets_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([create/1]).
-export([drop/1]).
-export([get/1]).
-export([put/1]).
-export([list/1]).
-export([delete/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        create,
        drop,
        get,
        put,
        list,
        delete
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ums),
    Config.

end_per_suite(_) ->
    ok = application:stop(ums),
    ok.

init_per_testcase(_, Config) ->
    ok = ums_datastore_ets:create(test_table),
    Config.

end_per_testcase(_, _Config) ->
    ums_datastore_ets:drop(test_table),
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

create(_) ->
    {error, already_exists, _} = ums_datastore_ets:create(test_table).

drop(_) ->
    ok = ums_datastore_ets:drop(test_table),
    {error, table_not_found, _} = ums_datastore_ets:list(test_table).

get(_) ->
    [] = ums_datastore_ets:get(test_table, <<"foo">>).

put(_) ->
    ok = ums_datastore_ets:put(test_table, <<"foo">>, bar),
    [{<<"foo">>, bar}] = ums_datastore_ets:get(test_table, <<"foo">>).

list(_) ->
    ok = ums_datastore_ets:put(test_table, <<"foo">>, bar),
    ok = ums_datastore_ets:put(test_table, <<"bar">>, baz),
    [{<<"foo">>, bar}, {<<"bar">>, baz}] = ums_datastore_ets:list(test_table).

delete(_) ->
    ok = ums_datastore_ets:put(test_table, <<"foo">>, bar),
    ok = ums_datastore_ets:delete(test_table, <<"foo">>),
    [] = ums_datastore_ets:get(test_table, <<"foo">>).

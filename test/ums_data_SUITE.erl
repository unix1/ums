-module(ums_data_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([create/1]).
-export([ensure_created/1]).
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
        ensure_created,
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
    ok = ums_data:init(),
    Config.

end_per_testcase(_, _Config) ->
    lists:foreach(fun(Module) -> Module:drop() end, all_data_modules()),
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

all_data_modules() ->
    [
        ums_data_org
    ].

%% ============================================================================
%% Tests
%% ============================================================================

create(_) ->
    lists:foreach(fun(Module) ->
            {error, already_exists, _} = Module:create(),
            ok = Module:drop(),
            ok = Module:create()
        end,
        all_data_modules()).

ensure_created(_) ->
    lists:foreach(fun(Module) ->
            {error, already_exists, _} = Module:create(),
            ok = Module:ensure_created()
        end,
        all_data_modules()).

drop(_) ->
    lists:foreach(fun(Module) ->
            ok = Module:drop(),
            {error, table_not_found, _} = Module:drop()
        end, all_data_modules()).

get(_) ->
    lists:foreach(fun(Module) ->
            [] = Module:get(<<"foo">>)
        end, all_data_modules()).

put(_) ->
    lists:foreach(fun(Module) ->
            [] = Module:get(<<"foo">>),
            ok = Module:put(<<"foo">>, bar),
            [{<<"foo">>, bar}] = Module:get(<<"foo">>)
        end, all_data_modules()).

list(_) ->
    lists:foreach(fun(Module) ->
            [] = Module:list(),
            ok = Module:put(<<"foo">>, bar),
            [{<<"foo">>, bar}] = Module:list(),
            ok = Module:put(<<"bar">>, baz),
            [{<<"foo">>, bar}, {<<"bar">>, baz}] = Module:list()
        end, all_data_modules()).

delete(_) ->
    lists:foreach(fun(Module) ->
            ok = Module:put(<<"foo">>, bar),
            ok = Module:delete(<<"foo">>),
            [] = Module:get(<<"foo">>)
        end, all_data_modules()).

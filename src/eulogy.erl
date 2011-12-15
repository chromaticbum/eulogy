-module(eulogy).

-include_lib("eunit/include/eunit.hrl").
-include("eulogy.hrl").

% API exports
-export([
    migrate_dir/1, migrate_dir/2,
    generate_migration/2
  ]).

-spec migrate_dir(Dir) -> {ok, migrated} | {error, Reason} when
  Dir :: filename(),
  Reason :: atom().

migrate_dir(Dir) ->
  DbInfo = eulogy_dir:db_info(Dir),
  migrate_dir(Dir, DbInfo).

-spec migrate_dir(Dir, DbInfo) -> {ok, migrated} | {error, Reason} when
  Dir :: filename(),
  DbInfo :: #db_info{},
  Reason :: atom().

migrate_dir(Dir, DbInfo) ->
  ok.

-spec generate_migration(Dir, Name) -> {ok, FileName} | {error, Reason} when
  Dir :: filename(),
  Name :: string(),
  FileName :: string(),
  Reason :: atom().

generate_migration(Dir, Name) ->
  {{Year, Month, Day}, {Hour, Minutes, Seconds}} = erlang:localtime(),
  Filename = io_lib:format("~s_~4.4.0p~2.2.0p~2.2.0p~2.2.0p~2.2.0p~2.2.0p",
    [Name, Year, Month, Day, Hour, Minutes, Seconds]),

  File = file:open(filename:join(Dir, Filename), write),
  file:close(File).

% TESTS

-define(TEST_DIR, "./test").

db_info1() ->
  #db_info{
    adapter = "test",
    user = "eulogy_test",
    password = "eulogy",
    host = "localhost",
    port = 3306,
    database = "eulogy_test"
  }.

generate_migration_test() ->
  generate_migration(?TEST_DIR, "add_records_table"),

  Files = list_dir(?TEST_DIR, "^add_records_table"),

  ?assertEqual(1, length(Files)),
  [File] = Files,
  ok = file:delete(filename:join(?TEST_DIR, File)).

list_dir(Dir, RegEx) ->
  {ok, Matcher} = re:compile(RegEx),
  {ok, Files} = file:list_dir(Dir),

  lists:filter(
    fun(File) ->
        case re:run(File, Matcher) of
          {match, _Captured} -> true;
          nomatch -> false
        end
    end, Files
  ).

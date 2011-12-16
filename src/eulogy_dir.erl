-module(eulogy_dir).

-include_lib("eunit/include/eunit.hrl").
-include("eulogy.hrl").

-define(CONFIG_FILENAME, "database.config").

% internal exports for eulogy
-export([
    db_info/1
  ]).

-spec db_info(Dir) -> {ok, DbInfo} | {error, Reason} when
  Dir :: filename(),
  DbInfo :: #db_info{},
  Reason :: atom().
db_info(Dir) ->
  db_info_file(filename:join(Dir, ?CONFIG_FILENAME)).

-spec db_info_file(File) -> {ok, DbInfo} | {error, Reason} when
  File :: filename(),
  DbInfo :: #db_info{},
  Reason :: atom().
db_info_file(File) ->
  case file:consult(File) of
    {ok, Terms} -> db_info_terms(Terms);
    {error, Reason} -> {error, Reason}
  end.

db_info_terms(Terms) ->
  [{database, Conf}] = Terms,

  Adapter = proplists:get_value(adapter, Conf),
  User = proplists:get_value(user, Conf),
  Password = proplists:get_value(password, Conf),
  Host = proplists:get_value(host, Conf),
  Port = proplists:get_value(port, Conf),
  Database = proplists:get_value(database, Conf),

  #db_info{
    adapter = Adapter,
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database
  }.

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

db_info_test() ->
  ?assertEqual(
    db_info1(),
    db_info(?TEST_DIR)
  ).

db_info_file_test() ->
  ?assertEqual(
    db_info1(),
    db_info_file(filename:join(?TEST_DIR, "database.config"))
  ),

  ?assertEqual(
    {error, enoent},
    db_info_file("badfile")
  ).

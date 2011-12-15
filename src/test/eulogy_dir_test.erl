-module(eulogy_dir_test).

-include_lib("eunit/include/eunit.hrl").
-include("eulogy.hrl").

-define(TEST_DIR, "./test").

-import(eulogy_dir,
  [db_info/1, db_info_file/1]
).

db_info1() ->
  #db_info{
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

-module(eulogy).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").
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
  prepare_statements(),
  start_connection(DbInfo),

  ensure_database(DbInfo),
  start_data_connection(DbInfo),

  ensure_migration_table(),

  run_migrations(Dir),

  stop_connections(),

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


-spec ensure_database(DbInfo) -> ok when
  DbInfo :: #db_info{}.
ensure_database(#db_info{database = Database}) ->
  emysql:execute(mysql_pool, list_to_binary(lists:concat(["create schema if not exists ", Database]))).


-spec ensure_migration_table() -> ok.
ensure_migration_table() ->
  emysql:execute(data_pool, <<"
    create table if not exists migrations (
        migration varchar(255) unique key
      );">>).


-spec run_migrations(Dir) -> ok when
  Dir :: filename().
run_migrations(Dir) ->
  Version = current_version(),
  Files = migration_files(Dir, Version),

  lists:foreach(
    fun(File) ->
        Migration = file:consult(filename:join(Dir, File)),
        case Migration of
          {ok, Conf} -> eulogy_migration:run(Conf, up);
          {error, Reason} -> ok
        end
    end, Files
  ),
  ok.


-spec migration_files(Dir, Version) -> [filename()] | {error, Reason} when
  Dir :: filename(),
  Version :: string(),
  Reason :: atom().
migration_files(Dir, Version) ->
  case list_dir(Dir, "^.*_(\\d{14,14})") of
    {error, Reason} -> {error, Reason};
    Files ->
      Versioned = version_migrations(Files),
      lists:filter(
        fun({Version2, _Migration}) -> (Version2 > Version) end,
        Versioned
      )
  end.

-spec version_migrations(Migrations) -> [filename()] when
  Migrations :: [filename()].
version_migrations(Migrations) ->
  {ok, Matcher} = re:compile("^.*_(\\d{14,14})"),

  lists:map(
    fun(Migration) ->
        case re:split(Migration, Matcher) of
          nomatch -> {"", Migration};
          MatchList ->
            Version = binary_to_list(lists:nth(2, MatchList)),
            {Version, Migration}
        end
    end, Migrations
  ).

-spec list_dir(Dir, RegEx) -> [filename()] | {error, Reason} when
  Dir :: filename(),
  RegEx :: string(),
  Reason :: atom().
list_dir(Dir, RegEx) ->
  {ok, Matcher} = re:compile(RegEx),

  case file:list_dir(Dir) of
    {ok, Files} ->
      lists:filter(
        fun(File) ->
            case re:run(File, Matcher) of
              {match, _Captured} -> true;
              nomatch -> false
            end
        end, Files
      );
    {error, Reason} -> {error, Reason}
  end.


-spec current_version() -> Version when
  Version :: string().
current_version() ->
  #result_packet{rows = Rows} = emysql:execute(data_pool, current_version, []),

  case Rows of
    [[Version]] -> binary_to_list(Version);
    [] -> ""
  end.


-spec update_version(Version) -> ok when
  Version :: integer().
update_version(Version) ->
  emysql:execute(data_pool, update_version, [Version]).


-spec start_connection(DbInfo) -> ok | {error, Reason} when
  DbInfo :: #db_info{},
  Reason :: atom().
start_connection(
  #db_info{
    user = User, password = Password,
    host = Host, port = Port
  }
) ->
  emysql:add_pool(mysql_pool, 1,
    User, Password, Host, Port,
    "mysql", utf8).


-spec start_data_connection(DbInfo) -> ok | {error, Reason} when
  DbInfo :: #db_info{},
  Reason :: atom().
start_data_connection(
  #db_info{
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database
  }
) ->
  emysql:add_pool(data_pool, 1,
    User, Password, Host, Port,
    Database, utf8).


-spec stop_connections() -> ok.
stop_connections() ->
  emysql:remove_pool(mysql_pool),
  emysql:remove_pool(data_pool),

  ok.

-spec prepare_statements() -> ok.
prepare_statements() ->
  emysql:prepare(current_version, <<"select migration from migrations order by migration desc limit 1;">>),
  emysql:prepare(update_version, <<"insert into migrations(migration) values(?);">>),

  ok.

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

start_connection_test() ->
  start_connection(db_info1()),
  emysql:increment_pool_size(mysql_pool, 1),
  emysql:remove_pool(mysql_pool).

ensure_database_test() ->
  DbInfo = db_info1(),
  prepare_statements(),
  start_connection(DbInfo),
  drop_database(DbInfo),

  ensure_database(DbInfo),

  start_data_connection(DbInfo),
  drop_database(DbInfo),
  stop_connections().

start_data_connection_test() ->
  DbInfo = db_info1(),
  prepare_statements(),
  start_connection(DbInfo),
  ensure_database(DbInfo),

  start_data_connection(DbInfo),

  emysql:increment_pool_size(data_pool, 1),
  stop_connections().

ensure_migration_table_test() ->
  DbInfo = db_info1(),
  prepare_statements(),
  start_connection(DbInfo),
  ensure_database(DbInfo),
  start_data_connection(DbInfo),

  ensure_migration_table(),

  #result_packet{} = emysql:execute(data_pool, <<"select migration from migrations">>),
  stop_connections().

current_and_update_version_test() ->
  DbInfo = db_info1(),
  bootstrap(DbInfo),

  update_version("1234"),
  update_version("2222"),
  update_version("1110"),

  ?assertEqual("2222", current_version()),

  shutdown().

version_migrations_test() ->
  Match = [
    {"11111111111101", "file_11111111111101"},
    {"11111111111102", "file_11111111111102"}
  ],
  Filenames = [
    "file_11111111111101",
    "file_11111111111102"
  ],

  ?assertMatch(Match, version_migrations(Filenames)).

migration_files_test() ->
  Versions = lists:map(
    fun({Version, _Migration}) -> Version end,
    migration_files(?TEST_DIR, "20110417123403")
  ),
  ?assertEqual(
    lists:sort(["20110417123404", "20110417123405"]),
    lists:sort(Versions)
  ).

generate_migration_test() ->
  generate_migration(?TEST_DIR, "add_records_table"),

  Files = list_dir(?TEST_DIR, "^add_records_table"),

  ?assertEqual(1, length(Files)),
  [File] = Files,
  ok = file:delete(filename:join(?TEST_DIR, File)).

drop_database(#db_info{database = Database}) ->
  emysql:execute(mysql_pool, list_to_binary(lists:concat(["drop schema if exists ", Database]))).

bootstrap(DbInfo) ->
  prepare_statements(),
  start_connection(DbInfo),
  ensure_database(DbInfo),
  start_data_connection(DbInfo),

  ensure_migration_table(),
  emysql:execute(data_pool, <<"start transaction;">>).

shutdown() ->
  emysql:execute(data_pool, <<"rollback;">>),
  stop_connections().


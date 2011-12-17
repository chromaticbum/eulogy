-module(eulogy).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").
-include("eulogy.hrl").

% API exports
-export([
    migrate_dir/1, migrate_dir/2,
    generate_migration/2
  ]).

-spec migrate_dir(Dir) -> ok | {error, Reason} when
  Dir :: filename(),
  Reason :: atom().
migrate_dir(Dir) ->
  case eulogy_dir:db_info(Dir) of
    {ok, DbInfo} -> migrate_dir(Dir, DbInfo);
    {error, Reason} -> {error, Reason}
  end.


-spec migrate_dir(Dir, DbInfo) -> ok when
  Dir :: filename(),
  DbInfo :: #db_info{}.
migrate_dir(Dir, DbInfo) ->
  Adapter = eulogy_adapter:create(DbInfo),
  run_migrations(Dir, Adapter),

  ok.


-spec generate_migration(Dir, Name) -> {ok, FileName} | {error, Reason} when
  Dir :: filename(),
  Name :: string(),
  FileName :: string(),
  Reason :: atom().
generate_migration(Dir, Name) ->
  {{Year, Month, Day}, {Hour, Minutes, Seconds}} = erlang:localtime(),
  Filename = io_lib:format("~s_~4.4.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w",
    [Name, Year, Month, Day, Hour, Minutes, Seconds]),

  case file:open(filename:join(Dir, Filename), write) of
    {ok, File} -> file:close(File);
    {error, Reason} -> {error, Reason}
  end.


-spec run_migrations(Dir, Adapter) -> ok when
  Dir :: filename(),
  Adapter :: #adapter{}.
run_migrations(Dir, Adapter) ->
  Version = eulogy_adapter:version(Adapter),
  Migrations = migrations(Dir, Version),

  error_logger:info_msg("Running migrations: ~p~n", [Migrations]),

  lists:foreach(
    fun(Migration) ->
        eulogy_migration:run(Adapter, Migration, up),
        eulogy_adapter:update_version(Adapter, Migration#migration.version)
    end, Migrations
  ),
  ok.


-spec migrations(Dir, Version) -> [{Version2, File}] | {error, Reason} when
  Dir :: filename(),
  Version :: string(),
  Version2 :: version(),
  File :: string(),
  Reason :: atom().
migrations(Dir, Version) ->
  case list_dir(Dir, "^.*_(\\d{14,14})") of
    {error, Reason} -> {error, Reason};
    Files ->
      Migrations = versioned_migrations(Dir, Files),
      lists:filter(
        fun(#migration{version = Version2}) -> (Version2 > Version) end,
        Migrations
      )
  end.

-spec versioned_migrations(Dir, Files) -> Migrations when
  Dir :: filename(),
  Files :: [filename()],
  Migrations :: migrations().
versioned_migrations(Dir, Files) ->
  {ok, Matcher} = re:compile("^.*_(\\d{14,14})"),

  lists:map(
    fun(File) ->
        case re:split(File, Matcher) of
          MatchList ->
            Version = binary_to_list(lists:nth(2, MatchList)),
            {ok, Instructions} = file:consult(filename:join(Dir, File)),
            #migration{
              version = Version,
              file = File,
              instructions = Instructions
            }
        end
    end, Files
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


% TESTS

-define(TEST_DIR, "./test").

db_info1() ->
  #db_info{
    adapter = test,
    user = "eulogy_test",
    password = "eulogy",
    host = "localhost",
    port = 3306,
    database = "eulogy_test"
  }.

versioned_migrations_test() ->
  Match = [
    #migration{version = "20110417123401", file = "create_a_records_table_20110417123401", instructions = []},
    #migration{version = "20110417123404", file = "add_length_to_records_table_20110417123404", instructions = []}
  ],
  Filenames = [
    "create_a_records_table_20110417123401",
    "add_length_to_records_table_20110417123404"
  ],

  ?assertEqual(Match, versioned_migrations(?TEST_DIR, Filenames)).

generate_migration_test() ->
  generate_migration(?TEST_DIR, "add_records_table"),

  Files = list_dir(?TEST_DIR, "^add_records_table"),

  ?assertEqual(1, length(Files)),
  [File] = Files,
  ok = file:delete(filename:join(?TEST_DIR, File)).


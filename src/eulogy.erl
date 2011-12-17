-module(eulogy).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").
-include("eulogy.hrl").

-define(MIGRATION_DIR, "eulogy").

-define(DEFAULT_TARGET_VERSION, "99999999999999").

% API exports
-export([
    migrate_app/2,
    migrate_dir/2,
    migrate/3,
    generate_migration/2
  ]).


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


-spec migrate_app(App, Conf) -> ok | {error, Reason} when
  App :: atom(),
  Conf :: [term()],
  Reason :: atom().
migrate_app(App, Conf) ->
  migrate_dir(filename:join(code:priv_dir(App), ?MIGRATION_DIR), Conf).


-spec migrate_dir(Dir, Conf) -> ok | {error, Reason} when
  Dir :: filename(),
  Conf :: [term()],
  Reason :: atom().
migrate_dir(Dir, Conf) ->
  case eulogy_dir:db_info(Dir) of
    {ok, DbInfo} -> migrate(Dir, DbInfo, Conf);
    {error, Reason} -> {error, Reason}
  end.


-spec migrate(Dir, DbInfo, Conf) -> ok when
  Dir :: filename(),
  DbInfo :: #db_info{},
  Conf :: [term()].
migrate(Dir, DbInfo, Conf) ->
  Adapter = eulogy_adapter:create(DbInfo),

  TargetVersion = proplists:get_value(target_version, Conf, ?DEFAULT_TARGET_VERSION),
  Version = eulogy_adapter:version(Adapter),

  Direction = case {(TargetVersion < Version), TargetVersion} of
    {_, ""} -> down;
    {true, _} -> down;
    {false, _} -> up
  end,

  Migrations = lists:sort(
    fun(#migration{version = Version2}, #migration{version = Version3}) ->
        case Direction of
          down -> Version2 >= Version3;
          up -> Version2 =< Version3
        end
    end, migrations(Dir, min(Version, TargetVersion), max(Version, TargetVersion))
  ),
  run_migrations(Adapter, Migrations, Direction),

  eulogy_adapter:stop(Adapter),

  ok.


-spec run_migrations(Adapter, Migrations, Direction) -> ok when
  Adapter :: #adapter{},
  Migrations :: migrations(),
  Direction :: migration_direction().
run_migrations(Adapter, Migrations, Direction) ->
  error_logger:info_msg("Running migrations: ~p~n", [Migrations]),

  lists:foreach(
    fun(Migration) ->
        eulogy_migration:run(Adapter, Migration, Direction)
    end, Migrations
  ),
  ok.


-spec migrations(Dir, Low, High) -> [{Version, File}] | {error, Reason} when
  Dir :: filename(),
  Low :: version(),
  High :: version(),
  Version :: version(),
  File :: string(),
  Reason :: atom().
migrations(Dir, Low, High) ->
  case list_dir(Dir, "^.*_(\\d{14,14})") of
    {error, Reason} -> {error, Reason};
    Files ->
      Versioned = versioned_migrations(Dir, Files),
      lists:filter(
        fun(#migration{version = Version}) -> ((Version > Low) andalso (Version =< High)) end,
        Versioned
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


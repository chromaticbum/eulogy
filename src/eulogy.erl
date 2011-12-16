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
  Adapter = create_adapter(DbInfo),
  run_migrations(Dir, Adapter),

  {ok, migrated}.


-spec create_adapter(DbInfo) -> Adapter when
  DbInfo :: #db_info{},
  Adapter :: #adapter{}.
create_adapter(#db_info{adapter = Adapter} = DbInfo) ->
  Module = list_to_atom(lists:concat(["eu_", Adapter])),

  #adapter{
    module = Module,
    info = Module:create(DbInfo)
  }.


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


-spec run_migrations(Dir, Adapter) -> ok when
  Dir :: filename(),
  Adapter :: #adapter{}.
run_migrations(Dir, #adapter{module = Module, info = Info} = Adapter) ->
  Version = Module:version(Info),
  Files = migration_files(Dir, Version),

  lists:foreach(
    fun({Version, File}) ->
        Migration = file:consult(filename:join(Dir, File)),
        case Migration of
          {ok, Conf} ->
            eulogy_migration:run(Adapter, Conf, up),
            Module:update_version(Info, Version);
          {error, Reason} -> ok
        end
    end, Files
  ),
  ok.


-spec migration_files(Dir, Version) -> [{Version2, File}] | {error, Reason} when
  Dir :: filename(),
  Version :: string(),
  Version2 :: version(),
  File :: string(),
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


% TESTS

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


-module(eulogy).

% API exports
-export([
    migrate_application/1, migrate_application/2,
    migrate_dir/1, migrate_dir/2,
    migrate_file/1, migrate_file/2
  ]).

% data types
-type filename()  :: string() | binary().

-type db_info() ::
  {user,        string()} |
  {password,    string()} |
  {host,        string()} |
  {port,        integer()}|
  {database,    string()}.

% records
-record(db_info, {
    user = "" :: string(),
    password = "" :: string(),
    host = "" :: string(),
    port = 0 :: integer(),
    database = "" :: string()
  }).

-spec migrate_application(App) -> {ok, migrated} | {error, Reason} when
  App :: atom(),
  Reason :: atom().

migrate_application(App) ->
  ok.

-spec migrate_application(App, DbInfo) -> {ok, migrated} | {error, Reason} when
  App :: atom(),
  DbInfo :: db_info(),
  Reason :: atom().

migrate_application(App, DbInfo) ->
  ok.

-spec migrate_dir(Dir) -> {ok, migrated} | {error, Reason} when
  Dir :: filename(),
  Reason :: atom().

migrate_dir(Dir) ->
  ok.

-spec migrate_dir(Dir, DbInfo) -> {ok, migrated} | {error, Reason} when
  Dir :: filename(),
  DbInfo :: db_info(),
  Reason :: atom().

migrate_dir(Dir, DbInfo) ->
  ok.

-spec migrate_file(File) -> {ok, migrated} | {error, Reason} when
  File :: filename(),
  Reason :: atom().

migrate_file(File) ->
  ok.

-spec migrate_dir(File, DbInfo) -> {ok, migrated} | {error, Reason} when
  File :: filename(),
  DbInfo :: db_info(),
  Reason :: atom().

migrate_file(File, DbInfo) ->
  ok.

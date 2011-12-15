-module(eulogy).

-include("eulogy.hrl").

% API exports
-export([
    migrate_application/1, migrate_application/2,
    migrate_dir/1, migrate_dir/2,
    migrate_file/1, migrate_file/2
  ]).

-spec migrate_application(App) -> {ok, migrated} | {error, Reason} when
  App :: atom(),
  Reason :: atom().

migrate_application(App) ->
  ok.

-spec migrate_application(App, DbInfo) -> {ok, migrated} | {error, Reason} when
  App :: atom(),
  DbInfo :: #db_info{},
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
  DbInfo :: #db_info{},
  Reason :: atom().

migrate_dir(Dir, DbInfo) ->
  ok.

-spec migrate_file(File) -> {ok, migrated} | {error, Reason} when
  File :: filename(),
  Reason :: atom().

migrate_file(File) ->
  ok.

-spec migrate_file(File, DbInfo) -> {ok, migrated} | {error, Reason} when
  File :: filename(),
  DbInfo :: #db_info{},
  Reason :: atom().

migrate_file(File, DbInfo) ->
  ok.

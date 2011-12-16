-module(eulogy_migration).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").
-include("eulogy.hrl").

% API exports
-export([
    run/2
  ]).

-type table() :: atom().
-type column() :: atom().
-type column_type() ::
  int |
  float |
  decimal |
  string |
  timestamp |
  datetime.

-type create_table() :: {create_table, table()}.
-type restore_table() :: {restore_table, table()}.
-type drop_table() :: {drop_table, table()}.
-type add_column() :: {add_column, {table(), column(), column_type()}}.
-type restore_column() :: {restore_column, {table(), column()}}.
-type remove_column() :: {remove_column, {table(), column(), column_type()}}.

-type migration_instruction() ::
  create_table() |
  restore_table() |
  drop_table() |
  add_column() |
  restore_column() |
  remove_column().

-type migration_direction() :: up | down.

-type migration() :: [migration_instruction()].

-spec run(Migration, Direction) -> ok when
  Migration :: migration(),
  Direction :: migration_direction().
run(Migration, Direction) ->
  case Direction of
    up -> run(Migration);
    down -> run(invert_migration(Migration))
  end.


-spec invert_migration(Migration) -> Migration2 when
  Migration :: migration(),
  Migration2 :: migration().
invert_migration(Migration) ->
  lists:reverse(
    lists:map(
      fun(Instruction) -> invert_instruction(Instruction) end,
      Migration
    )
  ).


-spec invert_instruction(Instruction) -> Instruction2 when
  Instruction :: migration_instruction(),
  Instruction2 :: migration_instruction().
invert_instruction({create_table, Table}) ->
  {drop_table, Table};
invert_instruction({drop_table, Table}) ->
  {restore_table, Table};
invert_instruction({add_column, {Table, Column, _Type}}) ->
  {remove_column, {Table, Column}};
invert_instruction({remove_column, {Table, Column}}) ->
  {restore_column, {Table, Column}}.


-spec run(Migration) -> ok when
  Migration :: migration().
run(Migration) ->
  lists:foreach(
    fun(Instruction) ->
        execute(Instruction)
    end, Migration
  ),

  ok.


-spec execute(Instruction) -> ok when
  Instruction :: migration_instruction().
execute({create_table, Table}) ->
  ok;
execute({drop_table, Table}) ->
  ok;
execute({add_column, {Table, Column, Type}}) ->
  ok;
execute({remove_column, {Table, Column, Type}}) ->
  ok.

% TESTS

migration1() ->
  [
    {create_table, players},
    {drop_table, games},
    {add_column, {players, name, string}},
    {remove_column, {players, country}}
  ].

invert_migration_test() ->
  ?assertEqual(
    [
      {restore_column, {players, country}},
      {remove_column, {players, name}},
      {restore_table, games},
      {drop_table, players}
    ], invert_migration(migration1())
  ).

invert_instruction_test() ->
  ?assertEqual(
    {drop_table, players},
    invert_instruction({create_table, players})
  ),
  ?assertEqual(
    {restore_table, players},
    invert_instruction({drop_table, players})
  ),
  ?assertEqual(
    {remove_column, {players, name}},
    invert_instruction({add_column, {players, name, string}})
  ),
  ?assertEqual(
    {restore_column, {players, name}},
    invert_instruction({remove_column, {players, name}})
  ).

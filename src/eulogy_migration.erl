-module(eulogy_migration).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").
-include("eulogy.hrl").

% API exports
-export([
    run/3
  ]).

-spec run(Adapter, Migration, Direction) -> ok when
  Adapter :: #adapter{},
  Migration :: migration(),
  Direction :: migration_direction().
run(Adapter, Migration, Direction) ->
  case Direction of
    up -> run(Adapter, Migration);
    down -> run(Adapter, invert_migration(Migration))
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
invert_instruction({create_table, Table, _Columns}) ->
  {drop_table, Table};
invert_instruction({drop_table, Table}) ->
  {restore_table, Table};
invert_instruction({add_column, {Table, Column}}) ->
  {remove_column, {Table, element(1, Column)}};
invert_instruction({remove_column, {Table, Column}}) ->
  {restore_column, {Table, Column}}.


-spec run(Adapter, Migration) -> ok when
  Adapter :: #adapter{},
  Migration :: migration().
run(Adapter, Migration) ->
  lists:foreach(
    fun(Instruction) ->
        execute(Adapter, Instruction)
    end, Migration
  ),

  ok.


-spec execute(Adapter, Instruction) -> ok when
  Adapter :: #adapter{},
  Instruction :: migration_instruction().
execute(Adapter, Instruction) -> ok.

% TESTS

migration1() ->
  [
    {create_table, players, [{id, int, [primary]}]},
    {drop_table, games},
    {add_column, {players, {name, string}}},
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
    invert_instruction({create_table, players, [{id, int, [primary]}]})
  ),
  ?assertEqual(
    {restore_table, players},
    invert_instruction({drop_table, players})
  ),
  ?assertEqual(
    {remove_column, {players, name}},
    invert_instruction({add_column, {players, {name, string}}})
  ),
  ?assertEqual(
    {restore_column, {players, name}},
    invert_instruction({remove_column, {players, name}})
  ).

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
  io:format("DIR: ~p~n", [Direction]),

  Migration2 = case Direction of
    up -> Migration;
    down -> invert_migration(Migration)
  end,

  lists:foreach(
    fun(Instruction) -> run_instruction(Adapter, Migration2, Instruction, Direction) end,
    Migration2#migration.instructions
  ),

  ok.


-spec invert_migration(Migration) -> Migration2 when
  Migration :: migration(),
  Migration2 :: migration().
invert_migration(#migration{instructions = Instructions} = Migration) ->
  Instructions2 = lists:reverse(
    lists:map(
      fun(Instruction) -> invert_instruction(Instruction) end,
      Instructions
    )
  ),
  Migration#migration{instructions = Instructions2}.


-spec invert_instruction(Instruction) -> Instruction2 when
  Instruction :: migration_instruction(),
  Instruction2 :: migration_instruction_inverted().
invert_instruction({create_table, Table, _Columns}) ->
  {drop_table, Table};
invert_instruction({drop_table, Table}) ->
  {restore_table, Table};
invert_instruction({add_column, Table, Column}) ->
  {drop_column, Table, element(1, Column)};
invert_instruction({drop_column, Table, Column}) ->
  {restore_column, Table, Column}.


-spec run_instruction(Adapter, Migration, Instruction, Direction) -> ok when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instruction :: migration_instruction(),
  Direction :: migration_direction().
run_instruction(Adapter, Migration, Instruction, up) ->
  execute(Adapter, Migration, Instruction),
  eulogy_adapter:store_instruction(Adapter, Migration, Instruction);
run_instruction(Adapter, Migration, Instruction, down) ->
  execute(Adapter, Migration, Instruction),
  eulogy_adapter:delete_instruction(Adapter, Migration, Instruction).


-spec execute(Adapter, Migration, Instruction) -> ok when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instruction :: migration_instruction().
execute(Adapter, _Migration, {create_table, Table, Columns}) ->
  eulogy_adapter:create_table(Adapter, Table, Columns);
execute(Adapter, _Migration, {drop_table, Table}) ->
  eulogy_adapter:drop_table(Adapter, Table);
execute(Adapter, _Migration, {add_column, Table, Column}) ->
  eulogy_adapter:add_column(Adapter, Table, Column);
execute(Adapter, _Migration, {drop_column, Table, Column}) ->
  eulogy_adapter:drop_column(Adapter, Table, Column);
execute(Adapter, Migration, {restore_table, Table}) ->
  restore_table(Adapter, Migration, Table).


-spec restore_table(Adapter, Migration, Table) -> ok when
  Adapter :: #adapter{},
  Migration :: migration(),
  Table :: table().
restore_table(Adapter, #migration{version = Version} = Migration, Table) ->
  Instructions = eulogy_adapter:restore_table_instructions(Adapter, Version, Table),
  lists:foreach(fun(Instruction) -> execute(Adapter, Migration, Instruction) end, Instructions),
  ok.


% TESTS

migration1() ->
  #migration {
    version = "19880417123456",
    file = "bootstrap_19880417123456",
    instructions = [
      {create_table, players, [{id, int, [primary]}]},
      {drop_table, games},
      {add_column, players, {name, string}},
      {drop_column, players, country}
    ]
  }.

eu_test1() ->
  #adapter{
    module = eu_test,
    info = eu_test
  }.

execute_test() ->
  Adapter = eu_test1(),
  Migration = migration1(),
  ?assertEqual(ok, execute(Adapter, Migration, {create_table, players, [{id, int, [primary]}]})),
  ?assertEqual(ok, execute(Adapter, Migration, {drop_table, players})),
  ?assertEqual(ok, execute(Adapter, Migration, {add_column, players, {country, string}})),
  ?assertEqual(ok, execute(Adapter, Migration, {drop_column, players, country})).

invert_migration_test() ->
  #migration{instructions = Instructions} = invert_migration(migration1()),
  ?assertEqual(
    [
      {restore_column, players, country},
      {drop_column, players, name},
      {restore_table, games},
      {drop_table, players}
    ], Instructions
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
    {drop_column, players, name},
    invert_instruction({add_column, players, {name, string}})
  ),
  ?assertEqual(
    {restore_column, players, name},
    invert_instruction({drop_column, players, name})
  ).

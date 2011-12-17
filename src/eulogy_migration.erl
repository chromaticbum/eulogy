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


-spec run(Adapter, Migration) -> ok when
  Adapter :: #adapter{},
  Migration :: migration().
run(Adapter, #migration{instructions = Instructions} = Migration) ->
  lists:foreach(
    fun(Instruction) ->
        execute(Adapter, Instruction),
        eulogy_adapter:store_instruction(Adapter, Migration, Instruction)
    end, Instructions
  ),

  ok.


-spec execute(Adapter, Instruction) -> ok when
  Adapter :: #adapter{},
  Instruction :: migration_instruction().
execute(Adapter, {create_table, Table, Columns}) ->
  eulogy_adapter:create_table(Adapter, Table, Columns);
execute(Adapter, {drop_table, Table}) ->
  eulogy_adapter:drop_table(Adapter, Table);
execute(Adapter, {add_column, Table, Column}) ->
  eulogy_adapter:add_column(Adapter, Table, Column);
execute(Adapter, {drop_column, Table, Column}) ->
  eulogy_adapter:drop_column(Adapter, Table, Column).

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
  ?assertEqual(ok, execute(Adapter, {create_table, players, [{id, int, [primary]}]})),
  ?assertEqual(ok, execute(Adapter, {drop_table, players})),
  ?assertEqual(ok, execute(Adapter, {add_column, players, {country, string}})),
  ?assertEqual(ok, execute(Adapter, {drop_column, players, country})).

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

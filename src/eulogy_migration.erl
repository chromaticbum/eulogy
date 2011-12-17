-module(eulogy_migration).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").
-include("eulogy.hrl").

% API exports
-export([
    run/3
  ]).


-spec run(Adapter, Migration, Direction) -> ok | {error, string()} when
  Adapter :: #adapter{},
  Migration :: migration(),
  Direction :: migration_direction().
run(Adapter, Migration, Direction) ->
  Migration2 = case Direction of
    up -> Migration;
    down -> invert_migration(Migration)
  end,

  case ensure_instructions(Adapter, Migration2, Migration2#migration.instructions) of
    {error, Reason} -> {error, Reason};
    Instructions -> persist_instructions(Adapter, Migration2, Instructions, Direction)
  end.


-spec persist_instructions(Adapter, Migration, Instructions, Direction) -> ok when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instructions :: migration_instructions(),
  Direction :: migration_direction().
persist_instructions(Adapter, Migration, Instructions, up) ->
  lists:foreach(
    fun(Instruction) -> eulogy_adapter:store_instruction(Adapter, Migration, Instruction) end,
    Instructions
  );
persist_instructions(Adapter, Migration, Instructions, down) ->
  lists:foreach(
    fun(Instruction) -> eulogy_adapter:delete_instruction(Adapter, Migration, Instruction) end,
    Instructions
  ).



-spec invert_migration(Migration) -> Migration2 when
  Migration :: migration(),
  Migration2 :: migration().
invert_migration(#migration{instructions = Instructions} = Migration) ->
  Migration#migration{instructions = invert_instructions(Instructions)}.


-spec invert_instructions(Instructions) -> Instructions2 when
  Instructions :: migration_instructions(),
  Instructions2 :: migration_instructions().
invert_instructions(Instructions) ->
  lists:reverse(lists:map(
    fun(Instruction) -> invert_instruction(Instruction) end,
    Instructions
  )).


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
  {restore_column, Table, Column};
invert_instruction({restore_table, Table}) ->
  {drop_table, Table};
invert_instruction({restore_column, Table, Column}) ->
  {drop_column, Table, Column}.


-spec execute(Adapter, Migration, Instruction) -> ok | {error, string()} when
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
  restore_table(Adapter, Migration, Table);
execute(Adapter, Migration, {restore_column, Table, Column}) ->
  restore_column(Adapter, Migration, Table, Column).


-spec restore_table(Adapter, Migration, Table) -> ok | {error, string()} when
  Adapter :: #adapter{},
  Migration :: migration(),
  Table :: table().
restore_table(Adapter, #migration{version = Version} = Migration, Table) ->
  case eulogy_adapter:restore_table_instructions(Adapter, Version, Table) of
    {error, Reason} -> {error, Reason};
    Instructions ->
      case ensure_instructions(Adapter, Migration, Instructions) of
        {error, Reason} -> {error, Reason};
        _ -> ok
      end
  end.


-spec ensure_instructions(Adapter, Migration, Instructions) -> migration_instructions() | {error, string()} when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instructions :: migration_instructions().
ensure_instructions(Adapter, Migration, Instructions) ->
  case run_instructions(Adapter, Migration, Instructions) of
    {error, Reason, FailedInstructions} ->
      error_logger:error_msg("Rolling back (~s): ~p~n", [Reason, FailedInstructions]),
      run_instructions(Adapter, Migration, invert_instructions(FailedInstructions)),
      {error, Reason};
    Instructions2 -> Instructions2
  end.


-spec run_instructions(Adapter, Migration, Instructions) -> migration_instructions() | {error, string(), migration_instructions()} when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instructions :: migration_instructions().
run_instructions(Adapter, Migration, Instructions) ->
  run_instructions(Adapter, Migration, Instructions, []).


-spec run_instructions(Adapter, Migration, Instructions, AccInstructions) -> migration_instructions() | {error, string(), migration_instructions()} when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instructions :: migration_instructions(),
  AccInstructions :: migration_instructions().
run_instructions(_Adapter, _Migration, [], AccInstructions) ->
  lists:reverse(AccInstructions);
run_instructions(Adapter, Migration, [Instruction | Instructions], AccInstructions) ->
  case execute(Adapter, Migration, Instruction) of
    ok -> run_instructions(Adapter, Migration, Instructions, [Instruction | AccInstructions]);
    {error, Reason} -> {error, Reason, lists:reverse(AccInstructions)}
  end.


-spec restore_column(Adapter, Migration, Table, Column) -> ok | {error, string()} when
  Adapter :: #adapter{},
  Migration :: migration(),
  Table :: table(),
  Column :: column_name().
restore_column(Adapter, #migration{version = Version} = Migration, Table, Column) ->
  Instruction = eulogy_adapter:restore_column_instruction(Adapter, Version, Table, Column),
  execute(Adapter, Migration, Instruction).


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

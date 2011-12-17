-module(eulogy_adapter).

-include("eulogy.hrl").

-export([
    create/1,
    stop/1,
    version/1,
    store_instruction/3,
    delete_instruction/3,

    create_table/3,
    drop_table/2,
    add_column/3,
    drop_column/3,
    restore_table_instructions/3,
    restore_column_instruction/4
  ]).


-spec create(DbInfo) -> Adapter when
  DbInfo :: #db_info{},
  Adapter :: #adapter{}.
create(#db_info{adapter = Adapter} = DbInfo) ->
  Module = list_to_atom(lists:concat(["eu_", Adapter])),

  #adapter{
    module = Module,
    info = Module:create(DbInfo)
  }.


-spec stop(Adapter) -> ok when
  Adapter :: #adapter{}.
stop(#adapter{module = Module, info = Info}) ->
  Module:stop(Info).


-spec version(Adapter) -> Version when
  Adapter :: #adapter{},
  Version :: version().
version(#adapter{module = Module, info = Info}) ->
  Module:version(Info).


-spec store_instruction(Adapter, Migration, Instruction) -> ok when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instruction :: migration_instruction().
store_instruction(#adapter{module = Module, info = Info}, Migration, Instruction) ->
  Module:store_instruction(Info, Migration, Instruction).


-spec delete_instruction(Adapter, Migration, Instruction) -> ok when
  Adapter :: #adapter{},
  Migration :: migration(),
  Instruction :: migration_instruction().
delete_instruction(#adapter{module = Module, info = Info}, Migration, Instruction) ->
  Module:delete_instruction(Info, Migration, Instruction).


-spec create_table(Adapter, Table, Columns) -> ok when
  Adapter :: #adapter{},
  Table :: table(),
  Columns :: columns().
create_table(#adapter{module = Module, info = Info}, Table, Columns) -> 
  Module:create_table(Info, Table, Columns).


-spec drop_table(Adapter, Table) -> ok when
  Adapter :: #adapter{},
  Table :: table().
drop_table(#adapter{module = Module, info = Info}, Table) ->
  Module:drop_table(Info, Table).


-spec add_column(Adapter, Table, Columns) -> ok when
  Adapter :: #adapter{},
  Table :: table(),
  Columns :: column().
add_column(#adapter{module = Module, info = Info}, Table, Column) ->
  Module:add_column(Info, Table, Column).


-spec drop_column(Adapter, Table, Column) -> ok when
  Adapter :: #adapter{},
  Table :: table(),
  Column :: column_name().
drop_column(#adapter{module = Module, info = Info}, Table, Column) ->
  Module:drop_column(Info, Table, Column).


-spec restore_table_instructions(Adapter, Version, Table) -> ok when
  Adapter :: #adapter{},
  Version :: version(),
  Table :: table().
restore_table_instructions(#adapter{module = Module, info = Info}, Version, Table) ->
  Module:restore_table_instructions(Info, Version, Table).


-spec restore_column_instruction(Adapter, Version, Table, Column) -> ok when
  Adapter :: #adapter{},
  Version :: version(),
  Table :: table(),
  Column :: column_name().
restore_column_instruction(#adapter{module = Module, info = Info}, Version, Table, Column) ->
  Module:restore_column_instruction(Info, Version, Table, Column).

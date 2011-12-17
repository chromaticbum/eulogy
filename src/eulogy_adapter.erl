-module(eulogy_adapter).

-include("eulogy.hrl").

-export([
    version/1,
    update_version/2,

    create_table/3,
    drop_table/2,
    add_column/3,
    drop_column/3
  ]).


-spec version(Adapter) -> Version when
  Adapter :: #adapter{},
  Version :: version().
version(#adapter{module = Module, info = Info}) ->
  Module:version(Info).


-spec update_version(Adapter, Version) -> ok when
  Adapter :: #adapter{},
  Version :: version().
update_version(#adapter{module = Module, info = Info}, Version) ->
  Module:update_version(Info, Version).


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

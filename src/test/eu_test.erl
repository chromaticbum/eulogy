-module(eu_test).

% eu_mysql_adapter API exports
-export([
    create/1,
    version/1,
    update_version/2,
    create_table/3,
    drop_table/2,
    add_column/3,
    drop_column/3
  ]).

create(_DbInfo) -> test_adapter.
version(_Info) -> "1337".
update_version(_Info, _Version) -> ok.
create_table(_Info, _Table, _Columns) -> ok.
drop_table(_Info, _Table) -> ok.
add_column(_Info, _Table, _Column) -> ok.
drop_column(_Info, _Table, _Column) -> ok.


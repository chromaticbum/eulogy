-type create_table() :: {create_table, table(), columns()}.
-type restore_table() :: {restore_table, table()}.
-type drop_table() :: {drop_table, table()}.
-type add_column() :: {add_column, {table(), column()}}.
-type restore_column() :: {restore_column, {table(), column_name()}}.
-type remove_column() :: {remove_column, {table(), column_name()}}.

-type migration_instruction() ::
  create_table() |
  restore_table() |
  drop_table() |
  add_column() |
  restore_column() |
  remove_column().
-type migration_instructions() :: [migration_instruction()].

-type migration_direction() :: up | down.

% Records
-record(migration, {
    version = "" :: version(),
    file = "" :: filename(),
    instructions = [] :: migration_instructions()
  }).
-type migration() :: #migration{}.
-type migrations() :: [migration()].

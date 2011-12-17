% Database types
-type table() :: atom().

-type column_name() :: atom().
-type column_type() ::
  int |
  float |
  string |
  timestamp |
  datetime.
-type column_option() ::
  primary |
  unique |
  auto_increment |
  primary_id.
-type column_options() :: [column_option()].

-type column() ::
  {column_name(), column_type()} |
  {column_name(), column_type(), column_options()}.

-type columns() :: [column()].

% Migration types

-type create_table() :: {create_table, table(), columns()}.
-type restore_table() :: {restore_table, table()}.
-type drop_table() :: {drop_table, table()}.
-type add_column() :: {add_column, table(), column()}.
-type restore_column() :: {restore_column, table(), column_name()}.
-type drop_column() :: {drop_column, table(), column_name()}.

-type migration_instruction() ::
  create_table() |
  restore_table() |
  drop_table() |
  add_column() |
  restore_column() |
  drop_column().
-type migration_instructions() :: [migration_instruction()].

-type migration_instruction_inverted() ::
  restore_table() |
  restore_column() |
  drop_table() |
  drop_column().

-type migration_direction() :: up | down.

% Records
-record(migration, {
    version = "" :: version(),
    file = "" :: filename(),
    instructions = [] :: migration_instructions()
  }).
-type migration() :: #migration{}.
-type migrations() :: [migration()].

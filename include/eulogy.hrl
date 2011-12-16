% data types
-type filename()  :: string() | binary().
-type version() :: string().

-type adapter_info() :: any().

% Migration types
-type table() :: atom().

-type column_name() :: atom().
-type column_type() ::
  int |
  float |
  string |
  timestamp |
  datetime.
-type column_option() ::
  primary.
-type column_options() :: [column_option()].

-type column() ::
  {column_name(), column_type()} |
  {column_name(), column_type(), column_options()}.

-type columns() :: [column()].

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

-type migration_direction() :: up | down.

-type migration() :: [migration_instruction()].

% records
-record(db_info, {
    adapter = "" :: string(),
    user = "" :: string(),
    password = "" :: string(),
    host = "" :: string(),
    port = 0 :: integer(),
    database = "" :: string()
  }).

-record(adapter, {
    module :: module(),
    info :: adapter_info()
  }).


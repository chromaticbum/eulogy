% data types
-type filename()  :: string() | binary().
-type version() :: string().

-type adapter_info() :: any().

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
  primary.
-type column_options() :: [column_option()].

-type column() ::
  {column_name(), column_type()} |
  {column_name(), column_type(), column_options()}.

-type columns() :: [column()].

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

-include("eulogy_migration.hrl").

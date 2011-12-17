% data types
-type filename()  :: string() | binary().
-type version() :: string().

-type adapter_info() :: any().

% records
-record(db_info, {
    adapter :: atom(),
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

% data types
-type filename()  :: string() | binary().
-type version() :: string().

% records
-record(db_info, {
    adapter = "" :: string(),
    user = "" :: string(),
    password = "" :: string(),
    host = "" :: string(),
    port = 0 :: integer(),
    database = "" :: string()
  }).


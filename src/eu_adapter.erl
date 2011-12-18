-module(eu_adapter).

-export([behaviour_info/1]).

-spec behaviour_info(callbacks) ->[{atom(), integer()}].
behaviour_info(callbacks) ->
  [
    {create, 1},
    {stop, 1},
    {version, 1},
    {store_instruction, 3},
    {delete_instruction, 3},

    {create_table, 3},
    {drop_table, 2},
    {add_column, 3},
    {drop_column, 3},
    {restore_table_instructions, 3},
    {restore_column_instruction, 4}
  ].


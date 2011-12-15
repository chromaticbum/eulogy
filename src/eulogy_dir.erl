-module(eulogy_dir).

-include("eulogy.hrl").

-define(CONFIG_FILENAME, "database.config").

% internal exports for eulogy
-export([
    db_info/1,
    db_info_file/1
  ]).

-spec db_info(Dir) -> {ok, DbInfo} | {error, Reason} when
  Dir :: filename(),
  DbInfo :: #db_info{},
  Reason :: atom().

db_info(Dir) ->
  db_info_file(filename:join(Dir, ?CONFIG_FILENAME)).

db_info_file(File) ->
  case file:consult(File) of
    {ok, Terms} -> db_info_terms(Terms);
    {error, Reason} -> {error, Reason}
  end.

db_info_terms(Terms) ->
  [{database, Conf}] = Terms,

  User = proplists:get_value(user, Conf),
  Password = proplists:get_value(password, Conf),
  Host = proplists:get_value(host, Conf),
  Port = proplists:get_value(port, Conf),
  Database = proplists:get_value(database, Conf),

  #db_info{
    user = User,
    password = Password,
    host = Host,
    port = Port,
    database = Database
  }.

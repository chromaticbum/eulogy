-module(eulogy_app).
-behavior(application).

-export([
    start/2,
    stop/1
  ]).

start(_Type, _Args) ->
  {ok, self()}.

stop(_S) ->
  ok.



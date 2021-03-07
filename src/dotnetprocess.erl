-module(dotnetprocess).


-export([init/0]).


init() ->
  receive
  after 5000 ->
    io:format(user, "hi from erlang ~n", [])
  end,
  init().

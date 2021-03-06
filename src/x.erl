-module(x).

-export([go/0]).

go() ->
  { ok, Handle } = dotnet:create_bridge(),
  ok.



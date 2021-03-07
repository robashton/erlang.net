-module(x).

-export([go/0]).

go() ->
  { ok, Ref } = dotnet:load_hostfxr(),
  { ok, Handle } = dotnet:create_bridge(Ref),
  Handle.



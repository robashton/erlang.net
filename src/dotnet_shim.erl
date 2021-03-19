-module(dotnet_shim).

-export([callback/1]).

callback(Cb) ->
  {ok, Bridge} = dotnet_host_bridge:get_bridge(),
  dotnet:erlang_callback(Bridge, Cb, []).

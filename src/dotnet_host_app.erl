-module(dotnet_host_app).


-behaviour(application).

-export([ start/2, stop/1 ]).


start(_StartType, _Args) ->
  { ok, HostFxr } = dotnet:load_hostfxr(),
  dotnet_host_sup:start_link(HostFxr).

stop(_State) ->
  ok.

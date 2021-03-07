-module(dotnethost_app).


-behaviour(application).

-export([ start/2, stop/1 ]).


start(_StartType, _Args) ->
  { ok, HostFxr } = dotnet:load_hostfxr(),
  dotnethost_sup:start_link(HostFxr).

stop(_State) ->
  ok.

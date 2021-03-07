-module(dotnethost_app).


-behaviour(application).

-export([ start/2, stop/1 ]).


start(_StartType, _Args) ->
  { ok, HostFxr } = dotnet:load_hostfxr(),


  { ok, spawn_link(fun() -> app_loop() end) }.

stop(_State) ->
  ok.


app_loop() ->
  receive
  after 5000 ->
    io:format(user, "Tick ~n", [])
  end,
  app_loop().

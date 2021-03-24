-module(dotnet_shim).

-export([ callback/1
         ,start_link/2
         ,start_link/3
        ]).

callback(Cb) ->
  {ok, Bridge} = dotnet_host_bridge:get_bridge(),
  dotnet:erlang_callback(Bridge, Cb, []).

start_link(Assembly, Module) ->
  {ok, Bridge} = dotnet_host_bridge:get_bridge(),
  dotnet:run_app_from_assembly(Bridge, Assembly, Module).

start_link(Assembly, Module, Args) ->
  {ok, Bridge} = dotnet_host_bridge:get_bridge(),
  dotnet:run_app_from_assembly(Bridge, Assembly, Module, Args).

-module(dotnet_supervisor).

-export([ start_link/2
        ]).

-export([ init/1 ]).

start_link(Name, Init) ->
  dotnet_proclib:start_link(Name, supervisor, { Name, dotnet_supervisor, [ Init ]}).

init([Init]) ->
  {ok, Bridge } = dotnet_host_bridge:get_bridge(),
  case dotnet:erlang_callback(Bridge, Init, []) of
    { ok, { Flags, Children } } ->

      WithoutUndefineds = { maps:filter(fun (_, V) -> V /= undefined end, Flags), lists:map(fun(Child) ->
                                                                         maps:filter(fun (_, V) -> V /= undefined end, Child)
                                                                     end, Children) },

      { ok,  WithoutUndefineds }
  end.

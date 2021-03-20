-module(dotnet_supervisor).

-export([ start_link/2
        ]).

-export([ init/1 ]).

start_link(Name, Init) ->
  supervisor:start_link(Name, ?MODULE, [Init]).

init([Init]) ->
  io:format(user, "I'm in a supervisor with pid ~p~n", [ self() ]),
  {ok, Bridge } = dotnet_host_bridge:get_bridge(),
  case dotnet:erlang_callback(Bridge, Init, []) of
    { ok, { Flags, Children } } ->

      WithoutUndefineds = { maps:filter(fun (_, V) -> V /= undefined end, Flags), lists:map(fun(Child) ->
                                                                         maps:filter(fun (_, V) -> V /= undefined end, Child)
                                                                     end, Children) },

      { ok,  WithoutUndefineds }
  end.




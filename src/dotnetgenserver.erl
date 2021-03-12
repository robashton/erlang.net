-module(dotnetgenserver).

-export([ start_link/1 ]).

-export([ init/1 ]).

start_link(CallbackResource) ->
  { ok, Pid } = gen_server:start_link(?MODULE, CallbackResource, []),
  Pid.

init([CallbackResource]) ->
  {ok ,Bridge } = dotnethost_bridge:get_bridge(),
  dotnet:genserver_init(Bridge, CallbackResource).

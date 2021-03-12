-module(dotnetgenserver).

-export([ init/1 ]).

start_link(CallbackResource) ->
  { ok, Pid } = gen_server:start_link(?MODULE, CallbackResource, []),
  Pid.

init([CallbackResource]) ->
  dotnet:genserver_init(Bridge, CallbackResource).

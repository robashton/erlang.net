-module(dotnetgenserver).

-export([ start_link/1 ]).

-export([ init/1 ]).

-record(state,
        { bridge :: term()
        , ref :: reference()
        }).

start_link(CallbackResource) ->
  { ok, Pid } = gen_server:start_link(?MODULE, [CallbackResource], []),
  Pid.

init([CallbackResource]) ->
  {ok ,Bridge } = dotnethost_bridge:get_bridge(),
  case dotnet:erlang_callback(Bridge, CallbackResource, []) of
    { ok, Ref } ->
      { ok, #state { bridge = Bridge
             , ref = Ref
             } }
  end.


handle_info(Msg, State) ->
  %% So, we could just have explicit calls
  %% or we could pass in a pile of delegates on startup
  %% my mind is unclear
  %% dotnet:handle_info(Bridge, , []).
  %% vs
  %% dotnet:erlang_callback(Bridge, State#state.handle_info, [ Msg, State#state.ref ]);
  { noreply, State }.

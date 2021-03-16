-module(dotnet_genserver).

-export([ start_link/1 ]).

-export([ init/1
        , handle_info/2
        ]).

-record(state,
        { bridge :: term()
        , ref :: reference()
        , handle_info :: undefined | reference()
        }).

start_link(Callbacks) ->
  gen_server:start_link(?MODULE, [Callbacks], []).

init([#{ init := Init
      ,  handleinfo := HandleInfo
       }]) ->
  {ok ,Bridge } = dotnet_host_bridge:get_bridge(),
  case dotnet:erlang_callback(Bridge, Init, []) of
    { ok, Ref } ->
      { ok, #state { bridge = Bridge
                   , ref = Ref
                   , handle_info = HandleInfo
                   } }
  end.


handle_info(Msg, State = #state { handle_info = undefined }) ->
  io:format(user, ".NET gen server received message ~p but no handle_info implemented ~n", [ Msg ]),
  { noreply, State };

handle_info(Msg, State = #state { handle_info = HandleInfo
                                , bridge = Bridge
                                , ref = Ref
                                }) ->
  case dotnet:erlang_callback(Bridge, HandleInfo, { Msg, Ref }) of
    { noreply, Ref2 } ->
      { noreply, State#state { ref = Ref2 } }
  end.

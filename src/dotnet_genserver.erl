-module(dotnet_genserver).

-export([ start_link/1
        , start_link/2
        ]).

-export([ init/1
        , handle_info/2
        , handle_call/3
        , handle_cast/2
        , terminate/2
        ]).

-record(state,
        { bridge :: term()
        , ref :: reference()
        , handle_info :: undefined | reference()
        , handle_call :: undefined | reference()
        , handle_cast :: undefined | reference()
        , terminate :: undefined | reference()
        }).

start_link(Callbacks) ->
  gen_server:start_link(?MODULE, [Callbacks], []).

start_link(Name, Callbacks) ->
  gen_server:start_link(Name, ?MODULE, [Callbacks], []).

init([#{ init := Init
       , handleinfo := HandleInfo
       , handlecall := HandleCall
       , handlecast := HandleCast
       , terminate := Terminate
       }]) ->
  {ok, Bridge } = dotnet_host_bridge:get_bridge(),
  case dotnet:erlang_callback(Bridge, Init, []) of
    { ok, Ref } ->
      { ok, #state { bridge = Bridge
                   , ref = Ref
                   , handle_info = HandleInfo
                   , handle_call = HandleCall
                   , handle_cast = HandleCast
                   , terminate = Terminate
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

handle_call(Msg, _ReplyTo, State = #state { handle_call = undefined }) ->
  io:format(user, ".NET gen server received message ~p but no handle_call implemented ~n", [ Msg ]),
  { noreply, State };

handle_call(Msg, ReplyTo, State = #state { handle_call = HandleCall
                                         , bridge = Bridge
                                         , ref = Ref
                                }) ->
  case dotnet:erlang_callback(Bridge, HandleCall, { Msg, ReplyTo, Ref }) of
    { reply, Result, Ref2 } ->
      { reply, Result, State#state { ref = Ref2 } }
  end.

handle_cast(Msg, State = #state { handle_cast = undefined }) ->
  io:format(user, ".NET gen server received message ~p but no handle_cast implemented ~n", [ Msg ]),
  { noreply, State };

handle_cast(Msg, State = #state { handle_cast = HandleInfo
                                , bridge = Bridge
                                , ref = Ref
                                }) ->
  case dotnet:erlang_callback(Bridge, HandleInfo, { Msg, Ref }) of
    { noreply, Ref2 } ->
      { noreply, State#state { ref = Ref2 } }
  end.

terminate(_Reason, _State = #state { terminate = undefined }) ->
  ok;

terminate(Reason, _State = #state { bridge = Bridge, terminate = Terminate, ref = Ref }) ->
  dotnet:erlang_callback(Bridge, Terminate, { Reason, Ref }).

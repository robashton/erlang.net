-module(dotnet_host_bridge).

-behaviour(gen_server).


-define(SERVER, ?MODULE).

-export([ start_link/1
        , get_bridge/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


-record(state, { host_fxr :: term()
               , bridge :: term()
               , app :: term()
               }).
get_bridge() ->
  [{ bridge, Bridge }] = ets:lookup(control, bridge),
  { ok, Bridge }.

start_link(HostFxr) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [HostFxr], []).

init([HostFxr]) ->
  process_flag(trap_exit, true),


  {ok, Bridge} = dotnet:create_bridge(HostFxr),

  ets:new(control, [set, protected, named_table]),
  ets:insert(control, { bridge, Bridge }),

  {ok, #state{ host_fxr = HostFxr
             , bridge = Bridge
             }}.

handle_call(not_implemented, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(not_implemented, State) ->
  {noreply, State}.

handle_info({call_fn, Args ={ M, F, A }, Resource}, State = #state{ bridge = Bridge }) ->
  Result = erlang:apply(M, F, A),
  ok = dotnet:callback(Bridge, Resource, Result),
  {noreply, State};

handle_info({'EXIT', _, normal}, State = #state{ bridge = _Bridge }) ->
  {noreply, State};

handle_info(_Other, State = #state{ bridge = _Bridge }) ->
  io:format(user, "What ~p~n", [ _Other ]),
  {noreply, State}.

terminate(normal, _State) ->
  ok;

terminate(_Reason, _State) ->
  io:format("wtf exit?? ~p ~n", [ _Reason ]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


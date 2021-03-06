-module(dotnet_host_bridge).

-behaviour(gen_server).


-define(SERVER, ?MODULE).

-export([ start_link/1
        , get_bridge/0
        ,  get_owner_of_dispatch/1
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
               , caller_pool :: maps:map(pid(), pid())
               }).
get_bridge() ->
  [{ bridge, Bridge }] = ets:lookup(control, bridge),
  { ok, Bridge }.

get_owner_of_dispatch(Pid) ->
  gen_server:call(?SERVER, { get_owner_of_dispatch, Pid }).

start_link(HostFxr) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [HostFxr], []).

init([HostFxr]) ->
  {ok, Bridge} = dotnet:create_bridge(HostFxr),

  ets:new(control, [set, protected, named_table]),
  ets:insert(control, { bridge, Bridge }),

  {ok, #state{ host_fxr = HostFxr
             , bridge = Bridge
             , caller_pool = maps:new()
             }}.

handle_call({get_owner_of_dispatch, DispatchLoopPid}, _From, State = #state{ caller_pool = Pool}) ->

  List =  maps:to_list(Pool),
  { Owner, _ }  = lists:keyfind(DispatchLoopPid, 2, List),

  {reply, { ok, Owner }, State}.

handle_cast(not_implemented, State) ->
  {noreply, State}.

handle_info({call_fn, Caller, Args, Resource}, State = #state{ bridge = Bridge, caller_pool = CallerPool }) ->
  NewState = case maps:find(Caller, CallerPool) of
               {ok, Worker} ->
                 Worker ! { '$$call_fn', Args, Resource },
                 State;
               error ->
                 Pid = spawn_link(fun() -> dispatch_loop(Caller) end),
                 erlang:monitor(process, Caller),
                 Pid ! { '$$call_fn', Args, Resource },
                 State#state { caller_pool = maps:put(Caller, Pid, CallerPool) }
             end,
  {noreply, NewState};

handle_info({'DOWN', _, _, Caller, _ }, State = #state{ caller_pool = CallerPool }) ->
  NewState = case maps:find(Caller, CallerPool) of
               {ok, Worker} ->
                 Worker ! '$$stop',
                 State#state { caller_pool = maps:remove(Caller, CallerPool) };
               error ->
                 State
             end,
  {noreply, NewState};

handle_info({'EXIT', _, normal}, State = #state{ bridge = _Bridge }) ->
  {noreply, State};

handle_info(_Other, State = #state{ bridge = _Bridge }) ->
  {noreply, State}.

terminate(normal, State) ->
  stop_dispatchers(State),
  ok;

terminate(_Reason, State) ->
  stop_dispatchers(State),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

stop_dispatchers(State = #state { caller_pool = Pool }) ->
  lists:foreach(fun(Pid) ->
                    Pid ! '$$stop'
                end, maps:values(Pool)).

dispatch_loop(Owner) ->
  receive
    { '$$call_fn', { M, F, A }, Resource } ->
      { ok, Bridge } = get_bridge(),
      Result = erlang:apply(M, F, A),
      dotnet:callback(Bridge, Resource, Result),
      dispatch_loop(Owner);
    '$$stop' -> ok;
    Other ->
      io:format(user, "proxying message ~p~n", [ Other ]),
      Owner ! Other
  end.

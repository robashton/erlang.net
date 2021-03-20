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
  process_flag(trap_exit, true),

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
                 io:format(user, "Spawned a dispatch loop on pid ~p~n", [ Pid ]),
                 erlang:monitor(process, Caller),
                 Pid ! { '$$call_fn', Args, Resource },
                 State#state { caller_pool = maps:put(Caller, Pid, CallerPool) }
             end,
  {noreply, NewState};

handle_info({'DOWN', _, _, Caller, _ }, State = #state{ caller_pool = CallerPool }) ->
  NewState = case maps:find(Caller, CallerPool) of
               {ok, Worker} ->
                 io:format(user, "Killing worker with pid ~p because ~p died~n", [ Worker, Caller ]),
                 Worker ! '$$stop',
                 State#state { caller_pool = maps:remove(Caller, CallerPool) };
               error ->
                 io:format(user, "Somehow received a monitor down for a caller that we don't know about?? ~n", []),
                 State
             end,
  {noreply, NewState};

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

dispatch_loop(Owner) ->
  receive
    { '$$call_fn', { M, F, A }, Resource } ->
      { ok, Bridge } = get_bridge(),
      Result = erlang:apply(M, F, A),
      dotnet:callback(Bridge, Resource, Result),
      dispatch_loop(Owner);
    '$$stop' -> ok;
    Other -> Owner ! Other
  end.

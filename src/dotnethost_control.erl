-module(dotnethost_control).

-behaviour(gen_server).


-define(SERVER, ?MODULE).

-export([ start_link/1
        , get_bridge/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        , terminate/2
        , code_change/3]).


-record(state, { host_fxr :: term()
               , bridge :: term()
               , app :: term()
               }).
get_bridge() ->
  io:format(user, "doing a lookup~n", []),
  [{ bridge, Bridge }] = ets:lookup(control, bridge),
  io:format(user, "did a lookup~n", []),
  { ok, Bridge }.

start_link(HostFxr) ->
  io:format(user, "dotnethost_control start_link~n", []),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [HostFxr], []).

init([HostFxr]) ->
  io:format(user, "dotnethost_control init~n", []),
  process_flag(trap_exit, true),
  {ok, Bridge} = dotnet:create_bridge(HostFxr),

  ets:new(control, [set, protected, named_table]),
  ets:insert(control, {bridge, Bridge}),

  io:format(user, "Bridge created ~n", []),
  {ok, #state{ host_fxr = HostFxr
             , bridge = Bridge
             , app = undefind
             }, {continue, load}}.

handle_continue(load, State = #state { bridge = Bridge }) ->
  Self = self(),
  io:format(user, "Starting the .NET app ~n", []),
  %% This will either need monitoring/linking/etc, or spinning up in a separate sup
  %% if it dies, we need to die
  spawn_link(fun() ->
                 {ok, Pid} = dotnet:run_app_from_assembly(Bridge, <<"priv/testimpl.dll", 0>>),
                 Self ! { started, Pid }
             end),
  {noreply, State}.

handle_call(get_bridge, _From, State = #state{ bridge = Bridge }) ->
  {reply, { ok, Bridge }, State}.

handle_cast(not_implemented, State) ->
  {noreply, State}.

handle_info({call_fn, Args ={ M, F, A }, Resource}, State = #state{ bridge = Bridge }) ->
  Result = erlang:apply(M, F, A),
  ok = dotnet:callback(Bridge, Resource, Result),
  {noreply, State};

handle_info({started, Pid}, State) ->
  io:format(user, ".NET app was started ~n", []),
  {noreply, State#state { app = Pid } };

handle_info(Other, State = #state{ bridge = Bridge }) ->
  io:format(user, "Got a weird message from somewhere ~p~n", [Other]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format(user, "dotnethost_control terminate with reason ~p", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


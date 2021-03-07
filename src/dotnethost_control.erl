-behaviour(gen_server).

-export([
         start_link/0
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, { host_fxr :: term()
               , bridge :: term()
               , app :: term()
               }).

start_link(HostFxr) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], [HostFxr]).

init([HostFxr]) ->
  {ok, Bridge} = dotnet:create_bridge(HostFxr),
  { ok, App } = dotnet:load_app_from_assembly(<<"priv/testimpl.dll", 0>>),
  {ok, #state{ host_fxr = HostFxr
             , bridge = Bridge
             , app = App
             }}.

handle_call(not_implemented, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(not_implemented, State) ->
  {noreply, State}.

handle_info({call_fn, { M, F, A }, Resource}, State = #state{ app = App }) ->
  Result = erlang:apply(M, F, A),
  ok = dotnet:callback(Bridge, Resource, Result),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


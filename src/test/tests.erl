-module(tests).

-export([wait_for/2]).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(single_process_app, "TestImpl.Tests.SingleProcessApp").

wait_for(Name, 0) -> false;
wait_for(Name, Timeout) ->
  case whereis(Name) of
    undefined -> timer:sleep(1), wait_for(Name, Timeout-1);
    _ -> true
  end.

all_test_() ->
  with_bridge(lists:concat([basic_app:tests()
                          , type_round_trip:tests()
                          , record_round_trip:tests()
                          , basic_gen:tests()
                          , dotnet_code:tests()
                          , named_gen_servers:tests()
                          , supervisors:tests()
                          ])).

create_host() ->
  case ets:whereis(test_host) of
    undefined ->
       logger:set_primary_config(level, none),
      { ok, Host } = dotnet:load_hostfxr(),
      ets:new(test_host, [set, protected, named_table]),
      ets:insert(test_host, { host_fxr, Host }),
      Host;
    _ ->
      [{ host_fxr, Host }] =  ets:lookup(test_host, host_fxr),
      Host
  end.

start_bridge(HostFxr) ->
  { ok, Pid } = dotnet_host_bridge:start_link(HostFxr),
  { ok, Bridge } = dotnet_host_bridge:get_bridge(),
  { Pid, Bridge }.

stop_bridge({Pid, _}) ->
  gen_server:stop(Pid).


with_bridge(Tests) ->
  { spawn, [
            { setup
            , fun create_host/0
            , fun (HostFxr) ->
                  [
                   {foreach,
                    fun() -> start_bridge(HostFxr) end,
                    fun stop_bridge/1,
                    lists:map(fun ({Name, Test}) ->
                                  fun({_, Bridge}) ->
                                      [ { Name,  fun() -> Test(Bridge) end  } ]
                                  end
                              end, Tests)
                   }]
              end
            }
           ]}.


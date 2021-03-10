-module(basic_app_tests).

-include_lib("eunit/include/eunit.hrl").

create_host() ->
  { ok, HostFxr } = dotnet:load_hostfxr(),
  HostFxr.

start_bridge(HostFxr) ->
  { ok, Pid } = dotnethost_bridge:start_link(HostFxr),
  { ok, Bridge } = dotnethost_bridge:get_bridge(),
  { Pid, Bridge }.

stop_bridge({Pid, _}) ->
  gen_server:stop(Pid).

-define(test_assembly, "priv/testimpl.dll").
-define(single_process_app, "TestImpl.Tests.SingleProcessApp").


with_bridge(Fun) ->
  { spawn, [
            { setup
            , fun create_host/0
            , fun (HostFxr) -> [
                   {foreach,
                    fun() -> start_bridge(HostFxr) end,
                    fun stop_bridge/1,
                    [fun({_, Bridge}) -> Fun(Bridge) end]
                   }]
              end
            }
           ]}.


foo_test_() ->
  with_bridge(fun(Bridge) ->
                  [
                   { <<"Run an app from an assembly">>,
                     fun() ->
                         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?single_process_app),
                         ?assert(is_pid(Pid))
                     end },
                   { <<"Send a message and receive a message from a C# process">>,
                     fun() ->
                         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?single_process_app),
                         Pid ! { send_me, self(), hi },
                         receive
                           Msg -> ?assertEqual(hi, Msg)
                         end
                     end },
                   { <<"Send a message to a C# process that sits in a message loop">>,
                     fun() ->
                         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?single_process_app),
                         Pid ! { send_me, self(), hi },
                         Pid ! { send_me, self(), hi2 },
                         receive
                           Msg1 -> ?assertEqual(hi, Msg1)
                         end,
                         receive
                           Msg2 -> ?assertEqual(hi2, Msg2)
                         end
                     end },
                   { <<"A C# process that terminates when the message loop ends">>,
                     fun() ->
                         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?single_process_app),
                         Reference = erlang:monitor(process, Pid),
                         Pid ! bye,
                         receive
                           Msg ->
                             ?assertMatch({'DOWN', Reference, process, Pid, _}, Msg)
                         end
                     end }
                  ]
              end).

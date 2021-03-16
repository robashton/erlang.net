-module(basic_app).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(single_process_app, "TestImpl.Tests.SingleProcessApp").

-export([tests/0]).

tests() ->
  [
   { <<"Run an app from an assembly">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?single_process_app),
         ?assert(is_pid(Pid))
     end },
   { <<"Send a message and receive a message from a C# process">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?single_process_app),
         Pid ! { send_me, self(), hi },
         receive
           Msg -> ?assertEqual(hi, Msg)
         end
     end },
   { <<"Send a message to a C# process that sits in a message loop">>,
     fun(Bridge) ->
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
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?single_process_app),
         Reference = erlang:monitor(process, Pid),
         Pid ! bye,
         receive
           Msg ->
             ?assertMatch({'DOWN', Reference, process, Pid, _}, Msg)
         end
     end }
  ].

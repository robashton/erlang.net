-module(basic_gen).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.GenServerApp").

-export([tests/0]).

tests() ->
  [
   { <<"Spawn a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         ?assert(is_pid(Pid))
     end },
   { <<"Send a message to the handle_info of a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         Pid ! {"hello bob", self()},
         receive
           Msg -> ?assertEqual("hello joe", Msg)
         end
     end },
   { <<"Send a different message to the handle_info of a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         Pid ! {"hi", self()},
         receive
           Msg -> ?assertEqual("woah", Msg)
         end
     end },
   { <<"Send a message to the handle_call of a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         Result = gen_server:call(Pid, "hello bob"),
         ?assertEqual("hello joe", Result)
     end },
   { <<"Send a different message to the handle_call of a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         Result = gen_server:call(Pid, "hi"),
         ?assertEqual("nope", Result)
     end },
   { <<"Send a message to the handle_info of a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         gen_server:cast(Pid, {"hello bob", self()}),
         receive
           Msg -> ?assertEqual("hello joe", Msg)
         end
     end },
   { <<"Send a different message to the handle_info of a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         gen_server:cast(Pid, {"hi", self()}),
         receive
           Msg -> ?assertEqual("woah", Msg)
         end
     end },
   { <<"Stop a gen server and invoke the terminate handler">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         gen_server:call(Pid, {"store", self()}),
         gen_server:stop(Pid),
         receive
           Msg -> ?assertEqual(bye, Msg)
         end
     end }
  ].

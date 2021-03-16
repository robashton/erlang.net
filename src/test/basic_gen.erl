-module(basic_gen).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.GenServerApp").

-export([tests/0]).

tests() ->
  [
   { <<"Spawn a gen server">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         ?assert(is_pid(Pid))
     end },
   { <<"Send a message to the handle_info of a gen server">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         Pid ! {"hello bob", self()},
         receive
           Msg -> ?assertEqual("hello joe", Msg)
         end
     end },
   { <<"Send a different message to the handle_info of a gen server">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         Pid ! {"hi", self()},
         receive
           Msg -> ?assertEqual("boobs", Msg)
         end
     end }
  ].

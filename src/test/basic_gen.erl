-module(basic_gen).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.GenServerApp").

-export([tests/1]).

tests(Bridge) ->
  { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
  [
   { <<"Spawn a gen server">>,
     fun() ->
         ?assert(is_pid(Pid))
     end },
   { <<"Send a message to the handle_info of a gen server">>,
     fun() ->
         Pid ! {"hello bob", self()},
         receive
           Msg -> ?assertEqual("hello joe", Msg)
         end
     end },
   { <<"Send a different message to the handle_info of a gen server">>,
     fun() ->
         Pid ! {"hi", self()},
         receive
           Msg -> ?assertEqual("boobs", Msg)
         end
     end }
  ].

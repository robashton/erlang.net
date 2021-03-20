-module(named_gen_servers).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.NamedGenServersApp").

-export([tests/0]).

tests() ->
  [
   { <<"Can start named gen servers">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),

         tests:wait_for('named-one', 100),
         tests:wait_for('named-two', 100),

         One = whereis('named-one'),
         Two = whereis('named-two'),

         ?assert(is_pid(One)),
         ?assert(is_pid(Two)),
         Pid ! stop
     end }
  ].




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

         wait_for('named-one', 100),
         wait_for('named-two', 100),

         One = whereis('named-one'),
         Two = whereis('named-two'),

         ?assert(is_pid(One)),
         ?assert(is_pid(Two)),
         Pid ! stop
     end }
  ].


wait_for(Name, 0) -> false;
wait_for(Name, Timeout) ->
  case whereis(Name) of
    undefined -> timer:sleep(1), wait_for(Name, Timeout-1);
    _ -> true
  end.


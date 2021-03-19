-module(supervisors).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.SimpleSupApp").

-export([tests/0]).

tests() ->
  [
   { <<"Can start a supervisor">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         ?assert(is_pid(Pid))
     end }
  ].



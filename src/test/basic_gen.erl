-module(basic_gen).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.GenServerApp").

-export([tests/1]).

tests(Bridge) ->
  [
   { <<"Spawn a gen server">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         ?assert(is_pid(Pid))
     end }
  ].

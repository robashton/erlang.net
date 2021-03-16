-module(dotnet_code).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.CodeApp").

-export([tests/1]).

tests(Bridge) ->
  { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
  [
   { <<"Write a file using the Erlang API">>,
     fun() ->
         Filename = temp_filename(),
         Pid ! { write, self(), Filename, <<"hello world">> },
         receive
           Result ->
             {ok, Data } = file:read_file(Filename),
             ?assertEqual(ok, Result),
             ?assertEqual(<<"hello world">>, Data)
         end
     end }
  ].


temp_filename() ->
  {A,B,C}=now(),
  lists:flatten(io_lib:format("/tmp/~p.~p.~p.txt",[A,B,C])).


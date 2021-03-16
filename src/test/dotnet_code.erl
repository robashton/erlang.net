-module(dotnet_code).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.CodeApp").

-export([tests/0]).

tests() ->
  [
   { <<"Write a file using the Erlang API">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         Filename = temp_filename(),
         Pid ! { write, self(), Filename, <<"hello world">> },
         receive
           Result ->
             {ok, Data } = file:read_file(Filename),
             ?assertEqual(ok, Result),
             ?assertEqual(<<"hello world">>, Data)
         end
     end },
   { <<"Write a file over multiple calls using the Erlang API">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         Filename = temp_filename(),
         Pid ! { open, Filename },
         Pid ! { write, <<"blobone">> },
         Pid ! { write, <<"blobtwo">> },
         Pid ! { close, self() },
         receive
           Result ->
             {ok, Data } = file:read_file(Filename),
             ?assertEqual(ok, Result),
             ?assertEqual(<<"bloboneblobtwo">>, Data)
         end
     end }
  ].


temp_filename() ->
  {A,B,C}=now(),
  lists:flatten(io_lib:format("/tmp/~p.~p.~p.txt",[A,B,C])).


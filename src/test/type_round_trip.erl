-module(type_round_trip).

-include_lib("eunit/include/eunit.hrl").

-export([tests/0]).

-define(test_assembly, "priv/testimpl.dll").
-define(round_trip_app, "TestImpl.Tests.RoundTripApp").

round_trip(Bridge, Term) ->
  { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?round_trip_app),
  Pid ! { self(), Term },
  receive
    M -> ?assertEqual(Term, M)
  end.


tests() ->
  lists:map(fun({Name, Term}) ->
                { Name, fun(Bridge) -> round_trip(Bridge, Term) end }
            end,
            [ { <<"Round trip an Atom">>, hi },
              { <<"Round trip an Int32">>, 1337 },
              { <<"Round trip an Int64">>, 4000000000 },
              { <<"Round trip a string list">>, "hello joe" },
              { <<"Round trip a pid">>, self() },
              { <<"Round trip a homogeneous tuple2">>, { 10, 20 } },
              { <<"Round trip a heterogeneous tuple2">>, { 10, "woo" } },
              { <<"Round trip a homogeneous tuple3">>, { 10, 20, 30 } },
              { <<"Round trip a heterogeneous tuple3">>, { 10, "woo", self() } }
            ]).

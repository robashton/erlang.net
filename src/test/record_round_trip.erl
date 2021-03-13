-module(record_round_trip).

-include_lib("eunit/include/eunit.hrl").

-export([tests/1]).

-define(test_assembly, "priv/testimpl.dll").
-define(round_trip_app, "TestImpl.Tests.RecordRoundTripApp").

round_trip(Bridge, Term) ->
  { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?round_trip_app),
  Pid ! Term,
  receive
    M -> ?assertEqual(Term, M)
  end.


tests(Bridge) ->
  [ { <<"Round trip a record with a pile of crap on it">>,
      fun() ->
          Term = #{ one => "hi"
                  , two => hi
                  , owner => self()
                  , four => 1337
                  , five => 4000000000
                  , six => { "wow", "so", self() }
                  },
          { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?round_trip_app),
          Pid ! Term,
          receive
            M -> ?assertEqual(Term, M)
          end
      end
    }
  ].

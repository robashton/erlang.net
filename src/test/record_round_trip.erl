-module(record_round_trip).

-include_lib("eunit/include/eunit.hrl").

-export([tests/0]).

-define(test_assembly, "priv/testimpl.dll").
-define(round_trip_app, "TestImpl.Tests.RecordRoundTripApp").


tests() ->
  [ { <<"Round trip a record with a pile of crap on it">>,
      fun() ->
          Term = #{ one => "hi"
                  , two => hi
                  , owner => self()
                  , four => 1337
                  , five => 4000000000
                  , six => { "wow", "so", self() }
                  },
          { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?round_trip_app),
          Pid ! Term,
          receive
            M -> ?assertEqual(Term, M)
          end
      end
    }
  ].

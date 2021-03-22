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
     end },
   { <<"Supervisor is named correctly">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         NamedPid = whereis('primary-sup'),
         ?assertEqual(Pid, NamedPid)
     end },
   { <<"Supervisor has children with the names that they chose">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         C1 = whereis('genserver-one'),
         C2 = whereis('genserver-two'),
         C3 = whereis('genserver-three'),
         ?assert(is_pid(C1)),
         ?assert(is_pid(C2)),
         ?assert(is_pid(C3))
     end },
   { <<"Supervisor has running children with correct ids">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         [ {'c1', C1, _, _ },
           {'c2', C2, _, _ },
           {'c3', C3, _, _ }
         ] = lists:sort(supervisor:which_children(Pid)),

         ?assert(is_pid(C1)),
         ?assert(is_pid(C2)),
         ?assert(is_pid(C3))
     end },
   { <<"one_for_all restarts all children">>,
     fun(Bridge) ->
         { ok, Pid } = dotnet:run_app_from_assembly(Bridge, ?test_assembly, ?genserver_app),
         [ {_, C1_1, _, _ },
           {_, C2_1, _, _ },
           {_, C3_1, _, _ }
         ] = lists:sort(supervisor:which_children(Pid)),

         exit(C1_1, 'arse'),

         timer:sleep(500),

         tests:wait_for('genserver-one', 100),
         tests:wait_for('genserver-two', 100),
         tests:wait_for('genserver-three', 100),

         [ {_, C1_2, _, _ },
           {_, C2_2, _, _ },
           {_, C3_2, _, _ }
         ] = lists:sort(supervisor:which_children(Pid)),

         ?assertNotEqual(C1_1, C1_2),
         ?assertNotEqual(C2_1, C2_2),
         ?assertNotEqual(C3_1, C3_2)
     end }
  ].



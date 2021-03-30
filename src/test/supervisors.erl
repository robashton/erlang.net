-module(supervisors).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(genserver_app, "TestImpl.Tests.SimpleSupApp").

-export([tests/0]).

tests() ->
  [
   { <<"Can start a supervisor">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         ?assert(is_pid(Pid))
     end },
   { <<"Supervisor is named correctly">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         NamedPid = whereis('primary-sup'),
         ?assertEqual(Pid, NamedPid)
     end },
   { <<"Supervisor has children with the names that they chose">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         C1 = whereis('genserver-one'),
         C2 = whereis('genserver-two'),
         C3 = whereis('genserver-three'),
         ?assert(is_pid(C1)),
         ?assert(is_pid(C2)),
         ?assert(is_pid(C3))
     end },
   { <<"Supervisor has running children with correct ids">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         [ {'c1', C1, _, _ },
           {'c2', C2, _, _ },
           {'c3', C3, _, _ },
           {'s1', S1, _, _ }
         ] = lists:sort(supervisor:which_children(Pid)),

         ?assert(is_pid(C1)),
         ?assert(is_pid(C2)),
         ?assert(is_pid(C3)),
         ?assert(is_pid(S1))
     end },
   { <<"one_for_all restarts all children">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),
         [ {_, C1_1, _, _ },
           {_, C2_1, _, _ },
           {_, C3_1, _, _ },
           {_, S1_1, _, _ }
         ] = lists:sort(supervisor:which_children(Pid)),

         Ref = erlang:monitor(process, C1_1),
         sys:terminate(C1_1, 'arse'),
         receive
           {'DOWN', Ref, process, C1_1, arse} -> ok
         end,

         tests:wait_for(s1, 100),

         [ {_, C1_2, _, _ },
           {_, C2_2, _, _ },
           {_, C3_2, _, _ },
           {_, S1_2, _, _ }
         ] = lists:sort(supervisor:which_children(Pid)),

         ?assertNotEqual(C1_1, C1_2),
         ?assertNotEqual(C2_1, C2_2),
         ?assertNotEqual(C3_1, C3_2),
         ?assertNotEqual(S1_1, S1_2)
     end },
   { <<"one_for_all means the whole tree got nerfed yo">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),

         [ {_, C1, _, _ } | _ ] = supervisor:which_children(Pid),
         SubChildren = supervisor:which_children('secondary-sup'),

         Ref = erlang:monitor(process, C1),
         sys:terminate(C1, 'arse'),
         receive
           {'DOWN', _, process, C1, arse} -> ok
         end,

         tests:wait_for('secondary-sup', 100),
         SubChildren2 = supervisor:which_children('secondary-sup'),

         ?assertNotEqual(SubChildren, SubChildren2)
     end },
   { <<"one_for_one in a child sup means the error is contained">>,
     fun() ->
         { ok, Pid } = dotnet:run_app_from_assembly(?test_assembly, ?genserver_app),

         Children = supervisor:which_children('primary-sup'),

         [ {_, C1, _, _ } | Tail ] = lists:sort(supervisor:which_children('secondary-sup')),

         Ref = erlang:monitor(process, C1),
         sys:terminate(C1, 'arse'),
         receive
           {'DOWN', _, process, C1, arse} -> ok
         end,

         Children2 = supervisor:which_children('primary-sup'),

         [ {_, C1_2, _, _ } | Tail2 ]  = lists:sort(supervisor:which_children('secondary-sup')),

         ?assertNotEqual(C1_2, C1),
         ?assertEqual(Tail, Tail2),
         ?assertEqual(Children, Children2)
     end }
  ].



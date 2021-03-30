-module(hosting_dotnet).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(single_process_app, "TestImpl.Tests.Simpl").

-export([tests/0, init/1]).

tests() ->
  [
   { <<"Run a supervisor from an assembly">>,
     fun() ->
         {ok, Pid } = supervisor:start_link(?MODULE, []),
         Child = whereis('primary-sup'),
         ?assert(is_pid(Child))
     end },
   { <<"Run a gen server from an assembly">>,
     fun() ->
         {ok, Pid } = supervisor:start_link(?MODULE, []),

         [ {_, Gen, _, _} | _ ]  = lists:sort(supervisor:which_children(Pid)),

          Reply = gen_server:call(Gen, "hello bob"),

          ?assertEqual(Reply, "hello joe")
     end }
  ].

init([]) ->
  {ok, { #{ strategy => one_for_one }, [  #{ start => { dotnet_shim, start_link, [ "priv/testimpl.dll", "TestImpl.Tests.SimpleSupApp" ]}
                                          , id => dotnet_sup
                                          , type => supervisor
                                          , shutdown => infinity
                                          },
                                          #{ start => { dotnet_shim, start_link, [ "priv/testimpl.dll", "TestImpl.Tests.GenServerApp" ]}
                                          , id => dotnet_gen
                                          , type => worker
                                          }
                                       ]}}.

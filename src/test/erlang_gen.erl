-module(erlang_gen).


-export([ start_link/1
        , other_start_link/1
        , init/1
        ]).

other_start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).

start_link(Name) ->
  Self = self(),
  Worker = spawn_link(fun() ->
                 process_flag(trap_exit, true),
                 Self ! gen_server:start_link({local, Name}, ?MODULE, [Name], []),
                 receive
                   {unlink, Pid} -> unlink(Pid);
                   {'EXIT', _, Reason }  -> exit(self(), Reason)
                 end,
                 receive
                   Foo -> ok
                 end
             end),

  receive
    {ok, Pid } ->
      link(Pid),
 %%     Worker ! { unlink, Pid },
      {ok,  Pid }
  end.


init([Name]) ->
  io:format(user, "Started gen server with name ~p~n", [ Name ]),
  { ok, Name }.

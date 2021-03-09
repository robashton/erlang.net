-module(dotnetprocess).

-export([ init/1 ]).

init(Callback) ->
  io:format(user, "Setting up a process with a callback ~p ~n", [ Callback ]),
  spawn_link(fun() ->
                 io:format(user, "Getting bridge in new process~n", []),
                 { ok, Bridge } = dotnethost_control:get_bridge(),
                 io:format(user, "Got bridge~n", []),
                 Result = dotnet:process_init(Bridge, Callback),
                 io:format(user, "dotnetprocess: Process init done~n", []),
                 io:format(user, "Looping with ~p~n", [Result]),
                 loop(Bridge, Result)
             end).

loop(_, {finish, Return}) ->
  Return;

loop(Bridge, {'receive', Cb}) ->
  io:format(user, "Requested to do a receive~n", []),
  Result = receive
             Msg ->
               dotnet:process_msg(Bridge, Cb, Msg)
           end,
  loop(Bridge, Result);

loop(Bridge, {'receive', Timeout, Cb}) ->
  io:format(user, "Requested to do a receive with a timeout~n", []),
  Result = receive
             Msg ->
               dotnet:process_msg(Bridge, Cb, Msg)
           after Timeout ->
                   dotnet:process_timeout(Bridge, Cb)
           end,
  loop(Bridge, Result).

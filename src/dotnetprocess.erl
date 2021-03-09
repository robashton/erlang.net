-module(dotnetprocess).

-export([ init/1 ]).

init(Callback) ->
  io:format(user, "Setting up a process with a callback ~p ~n", [ Callback ]),
  spawn_link(fun() ->
                 { ok, Bridge } = dotnethost_control:get_bridge(),
                 Result = dotnet:process_init(Bridge, Callback),
                 loop(Bridge, Callback)
             end).

loop(_, {finish, Return}) ->
  Return;

loop(Bridge, {'receive', Cb}) ->
  Result = receive
             Msg ->
               dotnet:process_msg(Bridge, Cb, Msg)
           end,
  loop(Bridge, Result);

loop(Bridge, {'receive', Timeout, Cb}) ->
  Result = receive
             Msg ->
               dotnet:process_msg(Bridge, Cb, Msg)
           after Timeout ->
                   dotnet:process_timeout(Bridge, Cb)
           end,
  loop(Bridge, Result).

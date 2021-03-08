-module(dotnetprocess).

-export([ init/1 ]).

init(Handle) ->
  { ok, Bridge } = dotnet_control:get_bridge(),
  Result = dotnet:process_init(Bridge, Handle),
  loop(Bridge, Result).

loop(_, {finish, Return}) ->
  Return;

loop(Bridge, {receive, Cb}) ->
  Result = receive
             Msg ->
               Result = dotnet:process_msg(Bridge, Cb, Msg),
               loop(Result)
           end,
  loop(Bridge, Result);

loop(Bridge, {receive, Timeout, Cb}) ->
  Result = receive
             Msg ->
               dotnet:process_msg(Bridge, Cb, Msg)
           after Timeout ->
                   dotnet:process_timeout(Bridge, Cb)
           end,
  loop(Bridge, Result).

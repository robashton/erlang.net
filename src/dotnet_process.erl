-module(dotnet_process).

-export([ init/1 ]).

init(Callback) ->
  { ok, ActualOwnerPid } = dotnet_host_bridge:get_owner_of_dispatch(self()),
  spawn(fun() ->
            link(ActualOwnerPid),
            { ok, Bridge } = dotnet_host_bridge:get_bridge(),
            Result = dotnet:erlang_callback(Bridge, Callback, undefined),
            loop(Bridge, Result)
        end).

loop(_, {finish, Return}) ->
  Return;

loop(Bridge, {'receive', Cb}) ->
  Result = receive
             Msg ->
               dotnet:erlang_callback(Bridge, Cb, Msg)
           end,
  loop(Bridge, Result);

loop(Bridge, {'receive', Timeout, Cb}) ->
  Result = receive
             Msg ->
               dotnet:erlang_callback(Bridge, Cb, Msg)
           after Timeout ->
               dotnet:erlang_callback(Bridge, Cb, undefined)
           end,
  loop(Bridge, Result).

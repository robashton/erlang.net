-module(dotnetprocess).

-export([ init/1 ]).

init(Callback) ->
  spawn_link(fun() ->
                 { ok, Bridge } = dotnethost_bridge:get_bridge(),
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

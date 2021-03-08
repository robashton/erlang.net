-module(dotnetprocess).

-export([ init/1
        , 'receive'/0
        , 'receive'/1
        ]).

init(Handle) ->
  %% Now, this implies that we'll be doing a
  %% super long call into C and C# - is ERL_NIF_DIRTY_JOB_CPU_BOUND (IO actually?) going to actually cut it?
  dotnet:process_loop(Handle).

'receive'()
  receive
    Foo -> Foo
  end.

'receive'(Timeout)
  receive
    Foo -> Foo
  after Timeout -> timeout
  end.

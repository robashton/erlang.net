-module(dotnet).

-export([ create_bridge/0
        ]).

-on_load(init/0).


init() ->
  File = filename:join(code:priv_dir(dotnet), dotnet),
  ok = erlang:load_nif(File, 0).

create_bridge() ->
  erlang:nif_error("Nif not loaded").

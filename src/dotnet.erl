-module(dotnet).

-export([ launch_dotnet/0
        ]).

-on_load(init/0).


init() ->
  File = filename:join(code:priv_dir(dotnet), dotnet),
  ok = erlang:load_nif(File, 0).


launch_dotnet() ->
  erlang:nif_error(nif_library_not_loaded).



-module(dotnet).

-export([ create_bridge/1
        , load_hostfxr/0
        ]).

-on_load(init/0).

load_hostfxr() ->
  load_hostfxr_impl(<<"priv/cslib.runtimeconfig.json">>).

init() ->
  File = filename:join(code:priv_dir(dotnet), dotnet),
  ok = erlang:load_nif(File, 0).

create_bridge(HostFxr) ->
  erlang:nif_error("Nif not loaded").

load_hostfxr_impl(_RuntimeConfig) ->
  erlang:nif_error("Nif not loaded").

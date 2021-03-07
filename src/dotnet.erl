-module(dotnet).

-export([ create_bridge/1
        , load_hostfxr/0
        , run_app_from_assembly/2
        , callback/3
        ]).

-on_load(init/0).

load_hostfxr() ->
  load_hostfxr_impl(<<"priv/cslib.runtimeconfig.json", 0>>).

init() ->
  File = filename:join(code:priv_dir(dotnet), dotnet),
  ok = erlang:load_nif(File, 0).

create_bridge(_HostFxr) ->
  erlang:nif_error("Nif not loaded").

run_app_from_assembly(_Bridge, _AssemblyName) ->
  erlang:nif_error("Nif not loaded").

callback(_Bridge, _Resource, _Result) ->
  erlang:nif_error("Nif not loaded").

load_hostfxr_impl(_RuntimeConfig) ->
  erlang:nif_error("Nif not loaded").

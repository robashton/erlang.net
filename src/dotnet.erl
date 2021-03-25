-module(dotnet).

-export([ create_bridge/1
        , load_hostfxr/0
        , run_app_from_assembly/3
        , run_app_from_assembly/4
        , erlang_callback/3
        , callback/3
        ]).

-on_load(init/0).

load_hostfxr() ->
  Priv = code:priv_dir(dotnet),
  load_hostfxr_impl(filename:join(Priv, "Erlang.runtimeconfig.json")).


create_bridge(HostFxr) ->
  File = filename:join(code:priv_dir(dotnet), "Erlang.dll"),
  create_bridge(HostFxr, File).


init() ->
  File = filename:join(code:priv_dir(dotnet), liberldotnet),
  ok = erlang:load_nif(File, 0).

create_bridge(_HostFxr, _DllPath) ->
  erlang:nif_error("Nif not loaded").

run_app_from_assembly(Bridge, AssemblyName, TypeName) ->
  run_app_from_assembly(Bridge, AssemblyName, TypeName, []).

run_app_from_assembly(_Bridge, _AssemblyName, _TypeName, _Args) ->
  erlang:nif_error("Nif not loaded").

callback(_Bridge, _Resource, _Result) ->
  erlang:nif_error("Nif not loaded").

erlang_callback(_Bridge, _Cb, _Args) ->
  erlang:nif_error("Nif not loaded").

load_hostfxr_impl(_RuntimeConfig) ->
  erlang:nif_error("Nif not loaded").


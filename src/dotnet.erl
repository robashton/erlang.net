-module(dotnet).

-export([ create_bridge/1
        , load_hostfxr/0
        , run_app_from_assembly/3
        , process_init/2
        , process_msg/3
        , process_timeout/2
        , callback/3
        ]).

-on_load(init/0).

load_hostfxr() ->
  load_hostfxr_impl("priv/cslib.runtimeconfig.json").

init() ->
  File = filename:join(code:priv_dir(dotnet), dotnet),
  ok = erlang:load_nif(File, 0).

create_bridge(_HostFxr) ->
  erlang:nif_error("Nif not loaded").

run_app_from_assembly(_Bridge, _AssemblyName, _TypeName) ->
  erlang:nif_error("Nif not loaded").

callback(_Bridge, _Resource, _Result) ->
  erlang:nif_error("Nif not loaded").

process_init(_Bridge, Cb) ->
  erlang:nif_error("Nif not loaded").

process_msg(_Bridge, Cb, Msg) ->
  erlang:nif_error("Nif not loaded").

process_timeout(_Bridge, Cb) ->
  erlang:nif_error("Nif not loaded").

load_hostfxr_impl(_RuntimeConfig) ->
  erlang:nif_error("Nif not loaded").

#include <erl_nif.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <iostream>
#include <unistd.h>

#include <nethost.h>

#include "coreclr_delegates.h"
#include "hostfxr.h"
#include "guff.h"
#include "utils.h"
#include "types.h"

typedef void* GCHANDLE;

#define TRACE_ENABLE 1
#ifdef TRACE_ENABLE
    #define TRACE(...) {      \
      printf(__VA_ARGS__);   \
    }
#else
    #define TRACE(...) {}
#endif



ERL_NIF_TERM param_error(ErlNifEnv* env, const char_t* param) {
  return enif_make_tuple2(env,
            enif_make_atom(env, "invalid_param"),
            enif_make_atom(env, param));
}

static void bridge_resource_destroy(ErlNifEnv *env, void *obj)
{
  bridge_context *context = (bridge_context*) obj;
  if(context->gchandle) {
    context->return_gchandle(context->gchandle);
    context->gchandle = NULL;
  }
}

static void pointer_resource_destroy(ErlNifEnv *env, void *obj)
{
  pointer_resource *resource = (pointer_resource*) obj;
  if(resource->data) {
    resource->return_gchandle(resource->data);
    resource->data = NULL;
    resource->return_gchandle = NULL;
  }
}

uint8_t get_dotnet_load_assembly(const char_t *config_path, hostfxr_resource* hostfxr)
{
    // Load .NET Core
    void *load_assembly_and_get_function_pointer = NULL;
    hostfxr_handle cxt = NULL;

    int rc = hostfxr->init_fptr(config_path, NULL, &cxt);

    if (rc != 0 || cxt == NULL)
    {
        TRACE("Init failed: %d \n", rc);
        hostfxr->close_fptr(cxt);
        return 0;
    }

    rc = hostfxr->get_delegate_fptr(
        cxt,
        hdt_load_assembly_and_get_function_pointer,
        &load_assembly_and_get_function_pointer);

    if (rc != 0 || load_assembly_and_get_function_pointer == NULL)
        TRACE("Get delegate failed: %d", rc);

    hostfxr->close_fptr(cxt);
    hostfxr->load_assembly_and_get_function_pointer = (load_assembly_and_get_function_pointer_fn)load_assembly_and_get_function_pointer;
    return load_assembly_and_get_function_pointer != NULL;
}

uint8_t load_hostfxr(hostfxr_resource* hostfxr)
{
  char_t buffer[MAX_PATH];
  size_t buffer_size = sizeof(buffer) / sizeof(char_t);
  int rc = get_hostfxr_path(buffer, &buffer_size, NULL);

  if (rc != 0) {
    TRACE("Failed to find hostfxr path \n");
    return 0;
  }

  void *lib = load_library(buffer);
  hostfxr->init_fptr = (hostfxr_initialize_for_runtime_config_fn)get_export(lib, "hostfxr_initialize_for_runtime_config");
  hostfxr->get_delegate_fptr = (hostfxr_get_runtime_delegate_fn)get_export(lib, "hostfxr_get_runtime_delegate");
  hostfxr->close_fptr = (hostfxr_close_fn)get_export(lib, "hostfxr_close");
  return (hostfxr->init_fptr && hostfxr->get_delegate_fptr && hostfxr->close_fptr);
}

extern "C" {

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {

  nif_globals* globals = (nif_globals*)calloc(1, sizeof(nif_globals));

  *priv_data = (void*)globals;

  globals->fn = enif_open_resource_type(env, NULL, "DotNetFn", NULL, ERL_NIF_RT_CREATE, NULL);
  globals->hostfxr_resource = enif_open_resource_type(env, NULL, "HostFxrResource", NULL, ERL_NIF_RT_CREATE, NULL);
  globals->bridge_resource = enif_open_resource_type(env, NULL, "BridgeResource", bridge_resource_destroy, ERL_NIF_RT_CREATE, NULL);
  globals->callback_resource = enif_open_resource_type(env, NULL, "CallbackResource", NULL, ERL_NIF_RT_CREATE, NULL);
  globals->pointer_resource = enif_open_resource_type(env, NULL, "PointerResource", pointer_resource_destroy, ERL_NIF_RT_CREATE, NULL);

  return 0;
}


static ERL_NIF_TERM load_hostfxr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);

  char runtimeconfig[MAX_PATH];

  if(!enif_get_string(env, argv[0], runtimeconfig, MAX_PATH, ERL_NIF_LATIN1)) { return param_error(env, "runtimeconfig"); }

  hostfxr_resource* hostfxr = (hostfxr_resource*)enif_alloc_resource(globals->hostfxr_resource, sizeof(hostfxr_resource));

  if(!load_hostfxr(hostfxr)) {
    enif_release_resource(hostfxr);
    TRACE("Completely failed to load hostfxr \n");
    return -1;
  }

  if(!get_dotnet_load_assembly(runtimeconfig, hostfxr)) {
    enif_release_resource(hostfxr);
    TRACE("Failed to get LoadAssembly \n");
    return -1;
  }

  ERL_NIF_TERM resource = enif_make_resource(env, hostfxr);
  enif_release_resource(hostfxr);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), resource);
}


static ERL_NIF_TERM create_bridge(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  hostfxr_resource* hostfxr;

  int rc = 0;
  if(!enif_get_resource(env, argv[0], globals->hostfxr_resource, (void**)&hostfxr)) { return param_error(env, "hostfxr"); }

  component_entry_point_fn fn;

  if(rc = hostfxr->load_assembly_and_get_function_pointer("priv/cslib.dll", "CsLib.Bridge, CsLib", "Create", NULL, NULL, (void**)&fn)) {
    TRACE("Sad trombone %d \n", rc);
    return enif_make_atom(env, "nope");
  }

  bridge_context* context = (bridge_context*)enif_alloc_resource(globals->bridge_resource, sizeof(bridge_context));
  memset(context, 0, sizeof(bridge_context));

  int result = fn(context, sizeof(bridge_context*));

  if(result) {
    enif_release_resource(context);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "create_failed"));
  }

  ERL_NIF_TERM resource = enif_make_resource(env, context);
  enif_release_resource(context);

  enif_self(env, &globals->owner);
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), resource);
}

static ERL_NIF_TERM run_app_from_assembly(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  bridge_context* context;
  char assemblyName[MAX_PATH];
  char typeName[MAX_PATH];

  int rc = 0;
  if(!enif_get_resource(env, argv[0], globals->bridge_resource, (void**)&context)) { return param_error(env, "bridge_resource"); }
  if(!enif_get_string(env, argv[1], assemblyName, MAX_PATH, ERL_NIF_LATIN1)) { return param_error(env, "assemblyName"); }
  if(!enif_get_string(env, argv[2], typeName, MAX_PATH, ERL_NIF_LATIN1)) { return param_error(env, "typeName"); }

  ERL_NIF_TERM result = context->run_app_from_assembly(env, context->gchandle, assemblyName, typeName);

  if(result) {
    return result;
  } else {
    return enif_make_atom(env, "error");
  }

}

static ERL_NIF_TERM callback(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  bridge_context* context;
  callback_resource* callback_resource;
  ErlNifBinary assemblyName;

  int rc = 0;
  if(!enif_get_resource(env, argv[0], globals->bridge_resource, (void**)&context)) { return param_error(env, "bridge_resource"); }
  if(!enif_get_resource(env, argv[1], globals->callback_resource, (void**)&callback_resource)) { return param_error(env, "callback_resource"); }

  callback_resource->result = argv[2];
  callback_resource->complete = 1;

  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM erlang_callback(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  bridge_context* context;
  pointer_resource* fn_ptr;

  if(!enif_get_resource(env, argv[0], globals->bridge_resource, (void**)&context)) { return param_error(env, "bridge_resource"); }

  return context->erlang_callback(env, context->gchandle, argv[1], argv[2]);
}

static ErlNifFunc nif_funcs[] =
{
  // Core setup
  {"load_hostfxr_impl", 1, load_hostfxr},
  {"create_bridge", 1, create_bridge},
  {"run_app_from_assembly", 3, run_app_from_assembly, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"callback", 3, callback},
  {"erlang_callback", 3, erlang_callback},
};

ERL_NIF_INIT(dotnet,
    nif_funcs,
    load,  // load
    NULL,  // upgrade
    NULL,  // unload
    NULL   // reload - deprecated
    )

  }

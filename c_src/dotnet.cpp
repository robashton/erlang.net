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

typedef void* GCHANDLE;

typedef struct nif_globals_ {
  ErlNifResourceType *fn;
  ErlNifResourceType *hostfxr_resource;
  ErlNifResourceType *bridge_resource;
  ErlNifResourceType *callback_resource;
  ErlNifResourceType *pointer_resource;
  ErlNifPid owner;
} nif_globals;

typedef struct callback_resource_ {
  uint8_t complete;
  ERL_NIF_TERM result;
} callback_resource;

typedef struct pointer_resource_ {
  void* data;
} pointer_resource;

// these are callbacks we'll give to C# to invoke to get stuff out of us
// All of these callbacks (when we dig back through the callstack)
// will have been invoked via C# that originally got invoked by our C
// and that C will have an ErlNifEnv that is valid for the duration of that call
// So we'll thread it through rather than the alternative which would be to create
// a Process Independent Env and (presumably) constantly copy stuff about
typedef ERL_NIF_TERM (*spawn_fn)(ErlNifEnv* env, void* fn);
typedef ERL_NIF_TERM (*make_atom_fn)(ErlNifEnv* env, const char_t* input);
typedef ERL_NIF_TERM (*make_int_fn)(ErlNifEnv* env, int value);
typedef ERL_NIF_TERM (*make_tuple2_fn)(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b);
typedef ERL_NIF_TERM (*make_tuple3_fn)(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b, ERL_NIF_TERM c);
typedef ERL_NIF_TERM (*make_pointer_resource_fn)(ErlNifEnv* env, void* ptr);
typedef void* (*unpack_pointer_resource_fn)(ErlNifEnv* env, ERL_NIF_TERM arg);
typedef ERL_NIF_TERM (*release_pointer_resource_fn)(ErlNifEnv* env, ERL_NIF_TERM arg);

typedef struct erlang_runtime_ {
  spawn_fn spawn;
  make_atom_fn make_atom;
  make_int_fn make_int;
  make_tuple2_fn make_tuple2;
  make_tuple3_fn make_tuple3;
  make_pointer_resource_fn make_pointer_resource;
  unpack_pointer_resource_fn unpack_pointer_resource;
  release_pointer_resource_fn release_pointer_resource;
} erlang_runtime;

// Callbacks that C# will give us by populating the struct
typedef void (*return_gchandle_fn)(GCHANDLE handle);
typedef ERL_NIF_TERM (*run_app_from_assembly_fn)(ErlNifEnv* env, GCHANDLE handle, const char_t* assemblyName);
typedef ERL_NIF_TERM (*process_init_fn)(ErlNifEnv* env, GCHANDLE handle, void* fn);
typedef ERL_NIF_TERM (*process_msg_fn)(ErlNifEnv* env, GCHANDLE handle, ERL_NIF_TERM fn, ERL_NIF_TERM msg);
typedef ERL_NIF_TERM (*process_timeout_fn)(ErlNifEnv* env, GCHANDLE handle, ERL_NIF_TERM fn);

typedef struct bridge_context_ {
  void* gchandle;
  erlang_runtime* runtime;
  return_gchandle_fn return_gchandle;
  run_app_from_assembly_fn run_app_from_assembly;
  process_init_fn process_init;
  process_msg_fn process_msg;
  process_timeout_fn process_timeout;
} bridge_context;

typedef struct hostfxr_resource_ {
  hostfxr_initialize_for_runtime_config_fn init_fptr;
  hostfxr_get_runtime_delegate_fn get_delegate_fptr;
  hostfxr_close_fn close_fptr;
  load_assembly_and_get_function_pointer_fn load_assembly_and_get_function_pointer;
} hostfxr_resource;


typedef struct dotnet_entry_point_ {
  component_entry_point_fn fn;
} dotnet_entry_point;


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

  if(context->runtime) {
    free(context->runtime);
    context->runtime = NULL;
  }
}

static void callback_resource_destroy(ErlNifEnv *env, void *obj)
{
  callback_resource *resource = (callback_resource*) obj;
}


static ERL_NIF_TERM call_erlang_fn(ErlNifEnv* env, ERL_NIF_TERM mfa) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);

  // We'll be nice and do this as a resource so we can pass it to erlang and back to us
  callback_resource* callback = (callback_resource*)enif_alloc_resource(globals->callback_resource, sizeof(callback_resource));
  memset(callback, 0, sizeof(callback_resource));

  // We'll not release it, cos Erlang will think it's finished with as soon as the callback is invoked
  ERL_NIF_TERM resource = enif_make_resource(env, callback);

  printf("Sending up to erlang \n");
  enif_send(NULL, &globals->owner, env, enif_make_tuple3(env,
        enif_make_atom(env, "call_fn"),
        mfa,
        resource)
      );

  printf("Spinning \n");

  // cheeky spin-wait on this being written in the nif:callback
  while(!callback->complete) {
    sleep(1);
  }

  printf("Spun \n");

  // TODO: This probably needs copying into our env?
  // as it originally came from our 'dotnethost_control' process
  ERL_NIF_TERM result = callback->result;

  // Now we can clear that resource up
  enif_release_resource(callback);

  // Finally, send this back to C# (lol)
  return result;

}


static ERL_NIF_TERM runtime_spawn(ErlNifEnv* env, void* fn) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);

  pointer_resource* ptr = (pointer_resource*)enif_alloc_resource(globals->pointer_resource, sizeof(pointer_resource));
  ptr->data = fn;
  ERL_NIF_TERM payload = enif_make_resource(env, ptr);

  printf("Sending up to erlang \n");
  ERL_NIF_TERM result = call_erlang_fn(env,
      enif_make_tuple3(env,
        enif_make_atom(env, "dotnetprocess"),
        enif_make_atom(env, "init"),
          enif_make_list1(env, payload)));

  enif_release_resource(ptr);

  return result;
}

static ERL_NIF_TERM runtime_make_atom(ErlNifEnv* env, const char_t* data) {
  return enif_make_atom(env, data);
}

static ERL_NIF_TERM runtime_make_int(ErlNifEnv* env, int value) {
  return enif_make_int(env, value);
}

static ERL_NIF_TERM runtime_make_tuple2(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b) {
  return enif_make_tuple2(env, a, b);
}

static ERL_NIF_TERM runtime_make_tuple3(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b, ERL_NIF_TERM c) {
  return enif_make_tuple3(env, a, b, c);
}

static ERL_NIF_TERM runtime_make_pointer_resource(ErlNifEnv* env, void* ptr) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* wrapper = (pointer_resource*)enif_alloc_resource(globals->pointer_resource, sizeof(pointer_resource));
  wrapper->data = ptr;
  return enif_make_resource(env, wrapper);
}

static void* runtime_unpack_pointer_resource(ErlNifEnv* env, ERL_NIF_TERM resource) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* ptr;

  if(enif_get_resource(env, resource, globals->pointer_resource, (void**)&ptr)) {
    return ptr->data;
  }
  return NULL;
}

static ERL_NIF_TERM runtime_release_pointer_resource(ErlNifEnv* env, ERL_NIF_TERM resource) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* ptr;

  if(enif_get_resource(env, resource, globals->pointer_resource, (void**)&ptr)) {
    enif_release_resource(ptr->data);
    ptr->data = NULL;
  }
  return NULL;
}

static erlang_runtime* create_runtime(ErlNifEnv* env) {
  erlang_runtime* runtime = (erlang_runtime*)calloc(1, sizeof(erlang_runtime));

  // Fns we want to provide to C#
  // In theory if we provide enough primitives here
  // it's possible we could delete a lot of C and do it all in C#
  runtime->spawn = &runtime_spawn;
  runtime->make_atom = &runtime_make_atom;
  runtime->make_int = &runtime_make_int;
  runtime->make_tuple2 = &runtime_make_tuple2;
  runtime->make_tuple3 = &runtime_make_tuple3;
  runtime->make_pointer_resource = &runtime_make_pointer_resource;
  runtime->unpack_pointer_resource = &runtime_unpack_pointer_resource;
  runtime->release_pointer_resource = &runtime_release_pointer_resource;
  return runtime;
}

uint8_t get_dotnet_load_assembly(const char_t *config_path, hostfxr_resource* hostfxr)
{
    // Load .NET Core
    void *load_assembly_and_get_function_pointer = NULL;
    hostfxr_handle cxt = NULL;

    int rc = hostfxr->init_fptr(config_path, NULL, &cxt);

    if (rc != 0 || cxt == NULL)
    {
        printf("Init failed: %d \n", rc);
        hostfxr->close_fptr(cxt);
        return 0;
    }

    rc = hostfxr->get_delegate_fptr(
        cxt,
        hdt_load_assembly_and_get_function_pointer,
        &load_assembly_and_get_function_pointer);

    if (rc != 0 || load_assembly_and_get_function_pointer == NULL)
        printf("Get delegate failed: %d", rc);

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
    printf("Failed to find hostfxr path \n");
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
  globals->callback_resource = enif_open_resource_type(env, NULL, "CallbackResource", callback_resource_destroy, ERL_NIF_RT_CREATE, NULL);
  globals->pointer_resource = enif_open_resource_type(env, NULL, "PointerResource", NULL, ERL_NIF_RT_CREATE, NULL);

  return 0;
}


static ERL_NIF_TERM load_hostfxr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);

  ErlNifBinary runtimeconfig;

  if(!enif_inspect_binary(env, argv[0], &runtimeconfig)) { return param_error(env, "runtimeconfig"); }

  hostfxr_resource* hostfxr = (hostfxr_resource*)enif_alloc_resource(globals->hostfxr_resource, sizeof(hostfxr_resource));

  if(!load_hostfxr(hostfxr)) {
    enif_release_resource(hostfxr);
    printf("Completely failed to load hostfxr \n");
    return -1;
  }

  if(!get_dotnet_load_assembly(reinterpret_cast<const char_t*>(runtimeconfig.data), hostfxr)) {
    enif_release_resource(hostfxr);
    printf("Failed to get LoadAssembly \n");
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
    printf("Sad trombone %d \n", rc);
    return enif_make_atom(env, "nope");
  }

  bridge_context* context = (bridge_context*)enif_alloc_resource(globals->bridge_resource, sizeof(bridge_context));
  memset(context, 0, sizeof(bridge_context));

  context->runtime = create_runtime(env);

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
  ErlNifBinary assemblyName;

  int rc = 0;
  if(!enif_get_resource(env, argv[0], globals->bridge_resource, (void**)&context)) { return param_error(env, "bridge_resource"); }
  if(!enif_inspect_binary(env, argv[1], &assemblyName)) { return param_error(env, "assemblyName"); }

  //const char* actual_data = (context->gchandle,reinterpret_cast<const char_t*>(assemblyName.data));

  ERL_NIF_TERM result = context->run_app_from_assembly(env, context->gchandle,reinterpret_cast<const char_t*>(assemblyName.data));

  if(result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
  } else {
    return enif_make_atom(env, "nope");
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

static ERL_NIF_TERM process_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  bridge_context* context;
  pointer_resource* fn_ptr;

  if(!enif_get_resource(env, argv[0], globals->bridge_resource, (void**)&context)) { return param_error(env, "bridge_resource"); }
  if(!enif_get_resource(env, argv[1], globals->pointer_resource, (void**)&fn_ptr)) { return param_error(env, "fn_ptr"); }

  printf("About to perform process init\r\n");
  auto result = context->process_init(env, context->gchandle, fn_ptr->data);
  printf("Process init done with result %lu\r\n", result);
  return enif_make_copy(env, result);
}

static ERL_NIF_TERM process_msg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  bridge_context* context;
  pointer_resource* fn_ptr;

  if(!enif_get_resource(env, argv[0], globals->bridge_resource, (void**)&context)) { return param_error(env, "bridge_resource"); }

  return context->process_msg(env, context->gchandle, argv[1], argv[2]);
}

static ERL_NIF_TERM process_timeout(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  bridge_context* context;
  pointer_resource* fn_ptr;

  if(!enif_get_resource(env, argv[0], globals->bridge_resource, (void**)&context)) { return param_error(env, "bridge_resource"); }

  return context->process_timeout(env, context->gchandle, argv[1]);
}

static ErlNifFunc nif_funcs[] =
{
  // Core setup
  {"load_hostfxr_impl", 1, load_hostfxr},
  {"create_bridge", 1, create_bridge},
  {"run_app_from_assembly", 2, run_app_from_assembly, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"callback", 3, callback},

  // Process API
  {"process_init", 2, process_init},
  {"process_msg", 3, process_msg},
  {"process_timeout", 2, process_timeout}
};

ERL_NIF_INIT(dotnet,
    nif_funcs,
    load,  // load
    NULL,  // upgrade
    NULL,  // unload
    NULL   // reload - deprecated
    )

  }

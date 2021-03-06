#include <erl_nif.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <iostream>


#include <nethost.h>

#include "coreclr_delegates.h"
#include "hostfxr.h"
#include "guff.h"

typedef struct nif_globals_ {
  hostfxr_initialize_for_runtime_config_fn init_fptr;
  hostfxr_get_runtime_delegate_fn get_delegate_fptr;
  hostfxr_close_fn close_fptr;
  load_assembly_and_get_function_pointer_fn load_assembly_and_get_function_pointer;
  ErlNifResourceType *fn;
} nif_globals;


typedef struct dotnet_entry_point_ {
  component_entry_point_fn fn;
} dotnet_entry_point;

uint8_t get_dotnet_load_assembly(const char_t *config_path, nif_globals* globals)
{
    // Load .NET Core
    void *load_assembly_and_get_function_pointer = nullptr;
    hostfxr_handle cxt = nullptr;
    
    printf("2 1 \r\n");
    
    int rc = globals->init_fptr(config_path, nullptr, &cxt);
    printf("2 2 \r\n");
    if (rc != 0 || cxt == nullptr)
    {
    printf("wut \r\n");
        std::cerr << "Init failed: " << std::hex << std::showbase << rc << std::endl;
        globals->close_fptr(cxt);
        return 0;
    }
    printf("2 3 \r\n");

    // Get the load assembly function pointer
    rc = globals->get_delegate_fptr(
        cxt,
        hdt_load_assembly_and_get_function_pointer,
        &load_assembly_and_get_function_pointer);
    printf("2 4 \r\n");
    if (rc != 0 || load_assembly_and_get_function_pointer == nullptr)
        std::cerr << "Get delegate failed: " << std::hex << std::showbase << rc << std::endl;

    printf("2 5 \r\n");
    globals->close_fptr(cxt);
    globals->load_assembly_and_get_function_pointer = (load_assembly_and_get_function_pointer_fn)load_assembly_and_get_function_pointer;
    return load_assembly_and_get_function_pointer != NULL;
}

uint8_t load_hostfxr(nif_globals* globals)
{
  char_t buffer[MAX_PATH];
  size_t buffer_size = sizeof(buffer) / sizeof(char_t);
  int rc = get_hostfxr_path(buffer, &buffer_size, NULL);

  if (rc != 0) {
    printf("Failed to find hostfxr path \r\n");
    return 0;
  }

  void *lib = load_library(buffer);
  globals->init_fptr = (hostfxr_initialize_for_runtime_config_fn)get_export(lib, "hostfxr_initialize_for_runtime_config");
  globals->get_delegate_fptr = (hostfxr_get_runtime_delegate_fn)get_export(lib, "hostfxr_get_runtime_delegate");
  globals->close_fptr = (hostfxr_close_fn)get_export(lib, "hostfxr_close");
  return (globals->init_fptr && globals->get_delegate_fptr && globals->close_fptr);
}

extern "C" {

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  nif_globals* globals = (nif_globals*)calloc(1, sizeof(nif_globals));

  *priv_data = (void*)globals;

  if(!load_hostfxr(globals)) {
    free(globals);
    printf("Completely failed to load hostfxr \r\n");
    return -1;
  } 

  if(!get_dotnet_load_assembly("priv/cslib.runtimeconfig.json", globals)) {
    free(globals);
    printf("Failed to get LoadAssembly \r\n");
    return -1;
  }


  globals->fn = enif_open_resource_type(env, NULL, "DotNetFn", NULL, ERL_NIF_RT_CREATE, NULL);

  return 0;
}


static ERL_NIF_TERM create_bridge(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  nif_globals* globals = (nif_globals*)(enif_priv_data(env));
  int rc = 0;

  component_entry_point_fn fn;

  if(rc = globals->load_assembly_and_get_function_pointer("priv/cslib.dll", "CsLib.Bridge, CsLib", "Create", NULL, NULL, (void**)&fn)) {
    printf("Sad trombone %d \r\n", rc);
    return enif_make_atom(env, "nope");
  }

  int result = fn(NULL, 0);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, result));
}


static ErlNifFunc nif_funcs[] =
{
  {"create_bridge", 0, create_bridge}
};

ERL_NIF_INIT(dotnet,
    nif_funcs,
    load,  // load
    NULL,  // upgrade
    NULL,  // unload
    NULL   // reload - deprecated
    )

  }


int main() {
  printf("hi \r\n");
  return 0;
}

#ifndef __ERLDOTNET_TYPES_H__

#include "coreclr_delegates.h"
#include "hostfxr.h"

typedef void* GCHANDLE;

typedef void (*return_gchandle_fn)(GCHANDLE handle);
typedef ERL_NIF_TERM (*run_app_from_assembly_fn)(ErlNifEnv* env, GCHANDLE handle, const char_t* assemblyName, const char_t* typeName, ERL_NIF_TERM args);
typedef ERL_NIF_TERM (*erlang_callback_fn)(ErlNifEnv* env, GCHANDLE handle, ERL_NIF_TERM fn, ERL_NIF_TERM args);

typedef struct bridge_context_ {
  GCHANDLE gchandle;
  return_gchandle_fn return_gchandle;
  run_app_from_assembly_fn run_app_from_assembly;
  erlang_callback_fn erlang_callback;
} bridge_context;
#define __ERLDOTNET_TYPES_H__

typedef struct nif_globals_ {
  ErlNifResourceType *fn;
  ErlNifResourceType *hostfxr_resource;
  ErlNifResourceType *bridge_resource;
  ErlNifResourceType *callback_resource;
  ErlNifResourceType *pointer_resource;
  ErlNifPid owner;
} nif_globals;



typedef struct hostfxr_resource_ {
  hostfxr_initialize_for_runtime_config_fn init_fptr;
  hostfxr_get_runtime_delegate_fn get_delegate_fptr;
  hostfxr_close_fn close_fptr;
  load_assembly_and_get_function_pointer_fn load_assembly_and_get_function_pointer;
} hostfxr_resource;


typedef struct dotnet_entry_point_ {
  component_entry_point_fn fn;
} dotnet_entry_point;

typedef struct pointer_resource_ {
  GCHANDLE data;
  return_gchandle_fn return_gchandle;
} pointer_resource;

typedef struct callback_resource_ {
  ErlNifCond* condition;
  ErlNifEnv* env;
  ERL_NIF_TERM result;
} callback_resource;

#endif // __ERLDOTNET_TYPES_H__


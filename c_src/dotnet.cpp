#include <erl_nif.h>

///#include <stdio.h>
//#include <stdint.h>
//#include <stdlib.h>
//#include <string.h>
//#include <assert.h>
//#include <iostream>


#include <nethost.h>

#include "coreclr_delegates.h"
#include "hostfxr.h"


//#ifdef WINDOWS
//#include <Windows.h>
//
//#define STR(s) L ## s
//#define CH(c) L ## c
//#define DIR_SEPARATOR L'\\'
//
//#else
//#include <dlfcn.h>
//#include <limits.h>
//
//#define STR(s) s
//#define CH(c) c
//#define DIR_SEPARATOR '/'
//#define MAX_PATH PATH_MAX
//
//#endif
//
//// Forward declarations
//void *load_library(const char_t *);
//void *get_export(void *, const char *);
//
//#ifdef WINDOWS
//void *load_library(const char_t *path)
//{
//  HMODULE h = ::LoadLibraryW(path);
//  assert(h != nullptr);
//  return (void*)h;
//}
//void *get_export(void *h, const char *name)
//{
//  void *f = ::GetProcAddress((HMODULE)h, name);
//  assert(f != nullptr);
//  return f;
//}
//#else
//void *load_library(const char_t *path)
//{
//  void *h = dlopen(path, RTLD_LAZY | RTLD_LOCAL);
//  assert(h != nullptr);
//  return h;
//}
//void *get_export(void *h, const char *name)
//{
//  void *f = dlsym(h, name);
//  assert(f != nullptr);
//  return f;
//}
//#endif
//
//
//uint8_t load_hostfxr()
//{
//    char_t buffer[2048];
//    size_t buffer_size = sizeof(buffer) / sizeof(char_t);
//    int rc = get_hostfxr_path(buffer, &buffer_size, null);
//    if (rc != 0)
//        return 0;
//
//    // Load hostfxr and get desired exports
//    void *lib = load_library(buffer);
//    hostfxr_initialize_for_runtime_config_fn init_fptr = (hostfxr_initialize_for_runtime_config_fn)get_export(lib, "hostfxr_initialize_for_runtime_config");
//    hostfxr_get_runtime_delegate_fn get_delegate_fptr = (hostfxr_get_runtime_delegate_fn)get_export(lib, "hostfxr_get_runtime_delegate");
//    hostfxr_close_fn close_fptr = (hostfxr_close_fn)get_export(lib, "hostfxr_close");
//
//    return (init_fptr && get_delegate_fptr && close_fptr);
//}


extern "C" {


static ERL_NIF_TERM launch_dotnet(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char_t buffer[2048];
    size_t buffer_size = sizeof(buffer) / sizeof(char_t);
    int rc = get_hostfxr_path(buffer, &buffer_size, NULL);

//  if(load_hostfxr()) {
//    return enif_make_atom(env, "ok");
//  } else {
    return enif_make_atom(env, "nope");
//  }

}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

static ErlNifFunc nif_funcs[] =
{
  {"launch_dotnet", 0, launch_dotnet}
};

ERL_NIF_INIT(dotnet,
    nif_funcs,
    load,  // load
    NULL,  // upgrade
    NULL,  // unload
    NULL   // reload - deprecated
    )

  }

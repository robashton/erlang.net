#include <erl_nif.h>

static ERL_NIF_TERM launch_dotnet(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  return enif_make_atom(env, "ok");
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

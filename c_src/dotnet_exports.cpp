#include <erl_nif.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>

extern "C" ERL_NIF_TERM erldotnet_write_debug(ErlNifEnv* env, const char* data) {
  printf("%s \r\n", data);
  return enif_make_atom(env, "ok");
}

extern "C" ERL_NIF_TERM erldotnet_make_atom(ErlNifEnv* env, const char* data) {
  return enif_make_atom(env, data);
}


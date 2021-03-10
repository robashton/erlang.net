#include <erl_nif.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>

#include "types.h"
#include "utils.h"

extern "C" ERL_NIF_TERM erldotnet_spawn(ErlNifEnv* env, void* fn) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);

  pointer_resource* ptr = (pointer_resource*)enif_alloc_resource(globals->pointer_resource, sizeof(pointer_resource));
  ptr->data = fn;
  ERL_NIF_TERM payload = enif_make_resource(env, ptr);

  ERL_NIF_TERM result = call_erlang_fn(env,
      enif_make_tuple3(env,
        enif_make_atom(env, "dotnetprocess"),
        enif_make_atom(env, "init"),
          enif_make_list1(env, payload)));

  enif_release_resource(ptr);

  return result;
}

extern "C" ERL_NIF_TERM erldotnet_write_debug(ErlNifEnv* env, const char* data) {
  printf("%s \r\n", data);
  return enif_make_atom(env, "ok");
}

extern "C" ERL_NIF_TERM erldotnet_make_atom(ErlNifEnv* env, const char* data) {
  return enif_make_atom(env, data);
}

extern "C" ERL_NIF_TERM erldotnet_make_int(ErlNifEnv* env, int32_t value) {
  return enif_make_int(env, value);
}

extern "C" ERL_NIF_TERM erldotnet_make_tuple2(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b) {
  return enif_make_tuple2(env, a, b);
}

extern "C" ERL_NIF_TERM erldotnet_make_tuple3(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b, ERL_NIF_TERM c) {
  return enif_make_tuple3(env, a, b, c);
}

extern "C" ERL_NIF_TERM erldotnet_make_pointer_resource(ErlNifEnv* env, void* ptr) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* wrapper = (pointer_resource*)enif_alloc_resource(globals->pointer_resource, sizeof(pointer_resource));
  wrapper->data = ptr;
  return enif_make_resource(env, wrapper);
}

extern "C" void* erldotnet_unpack_pointer_resource(ErlNifEnv* env, ERL_NIF_TERM resource) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* ptr;

  if(enif_get_resource(env, resource, globals->pointer_resource, (void**)&ptr)) {
    return ptr->data;
  }
  return NULL;
}

extern "C" ERL_NIF_TERM erldotnet_release_pointer_resource(ErlNifEnv* env, ERL_NIF_TERM resource) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* ptr;

  if(enif_get_resource(env, resource, globals->pointer_resource, (void**)&ptr)) {
    enif_release_resource(ptr->data);
    ptr->data = NULL;
  }
  return enif_make_atom(env, "ok");
}

extern "C" int erldotnet_string_or_atom_length(ErlNifEnv* env, ERL_NIF_TERM term) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* ptr;

  if(enif_is_atom(env, term)) {
    unsigned int len;
    enif_get_atom_length(env,term, &len, ERL_NIF_LATIN1);
    return len;
  }
  return -1;
}


extern "C" int erldotnet_term_to_string(ErlNifEnv* env, char* buffer, unsigned int buffer_len, ERL_NIF_TERM term) {
  if(enif_is_atom(env, term)) {
    int result = enif_get_atom(env, term, buffer, buffer_len, ERL_NIF_LATIN1);
  }
  return -1;
}

extern "C" uint8_t erldotnet_is_pid(ErlNifEnv* env,  ERL_NIF_TERM term) {
  return enif_is_pid(env, term) == 1;
}

extern "C" int erldotnet_tuple_length(ErlNifEnv* env,  ERL_NIF_TERM term) {
  int arity;
  const ERL_NIF_TERM* array;
  if(enif_get_tuple(env, term, &arity, &array)) {
    return arity;
  }
  else {
    return -1;
  };
}

extern "C" ERL_NIF_TERM erldotnet_tuple_element(ErlNifEnv* env, int index, ERL_NIF_TERM term) {
  int arity;
  const ERL_NIF_TERM* array;
  if(enif_get_tuple(env, term, &arity, &array)) {
    if(arity > index) {
      return array[index];
    }
  }
  return 0;
}

extern "C" int erldotnet_send(ErlNifEnv* env, ERL_NIF_TERM maybePid, ERL_NIF_TERM term) {
  ErlNifPid pid;
  if(enif_get_local_pid(env, maybePid, &pid)) {
    enif_send(env, &pid, NULL, term);
    return 1;
  }
  return -1;
}


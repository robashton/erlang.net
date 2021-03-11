#include <erl_nif.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>

#include "types.h"
#include "utils.h"

extern "C" ERL_NIF_TERM erldotnet_call_erlang_fn(ErlNifEnv* env, ERL_NIF_TERM mfa) {
  return call_erlang_fn(env, mfa);
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

extern "C" ERL_NIF_TERM erldotnet_make_list1(ErlNifEnv* env, ERL_NIF_TERM a) {
  return enif_make_list1(env, a);
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

extern "C" int erldotnet_send(ErlNifEnv* env, ErlNifPid pid, ERL_NIF_TERM term) {
  enif_send(env, &pid, NULL, term);
  return 1;
}

extern "C" uint8_t erldotnet_is_tuple(ErlNifEnv* env,  ERL_NIF_TERM term) {
  return enif_is_tuple(env, term) == 1;
}

extern "C" uint8_t erldotnet_is_pid(ErlNifEnv* env,  ERL_NIF_TERM term) {
  return enif_is_pid(env, term) == 1;
}


extern "C" uint8_t erldotnet_is_atom(ErlNifEnv* env,  ERL_NIF_TERM term) {
  return enif_is_atom(env, term) == 1;
}

extern "C" uint8_t erldotnet_is_double(ErlNifEnv* env,  ERL_NIF_TERM term) {
  return enif_term_type(env, term) == ERL_NIF_TERM_TYPE_FLOAT;
}


extern "C" uint8_t erldotnet_is_number(ErlNifEnv* env,  ERL_NIF_TERM term) {
  return enif_is_number(env, term) == 1;
}

extern "C" uint8_t erldotnet_is_int32(ErlNifEnv* env,  ERL_NIF_TERM term) {
  int num;
  return enif_get_int(env, term, &num) == 1;
}

extern "C" uint8_t erldotnet_is_int64(ErlNifEnv* env,  ERL_NIF_TERM term) {
  long int num;
  return enif_get_int64(env, term, &num) == 1;
}

extern "C" uint8_t erldotnet_is_string(ErlNifEnv* env,  ERL_NIF_TERM term) {
  unsigned int len;
  if(!enif_is_list(env, term)) return 0;
  if(!enif_get_list_length(env, term, &len)) return 0;

  char buffer[len +  1];
  if(enif_get_string(env, term, buffer, len + 1, ERL_NIF_LATIN1) <= 0) { return false; }
  return true;
}

extern "C" int erldotnet_string_or_atom_length(ErlNifEnv* env, ERL_NIF_TERM term) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);
  pointer_resource* ptr;

  unsigned int len;
  if(enif_is_atom(env, term)) {
    enif_get_atom_length(env,term, &len, ERL_NIF_LATIN1);
    return len;
  }
  if(erldotnet_is_string(env, term)) { 
    enif_get_list_length(env,term, &len);
    return len;
  }
  return -1;
}

extern "C" int erldotnet_term_to_string(ErlNifEnv* env, char* buffer, unsigned int buffer_len, ERL_NIF_TERM term) {
  if(enif_is_atom(env, term)) {
    return enif_get_atom(env, term, buffer, buffer_len, ERL_NIF_LATIN1);
  }
  if(enif_is_list(env, term)) {
    return enif_get_string(env, term, buffer, buffer_len, ERL_NIF_LATIN1);
  }
  return -1;
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

extern "C" ErlNifPid erldotnet_get_pid(ErlNifEnv* env,  ERL_NIF_TERM term) {
  ErlNifPid result;
  enif_get_local_pid(env, term, &result);
  return result;
}

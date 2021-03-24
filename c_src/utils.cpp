#include <erl_nif.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>

#include "types.h"

ERL_NIF_TERM call_erlang_fn(ErlNifEnv* env, ERL_NIF_TERM mfa) {
  nif_globals* globals = (nif_globals*)enif_priv_data(env);

  // We'll be nice and do this as a resource so we can pass it to erlang and back to us
  callback_resource* callback = (callback_resource*)enif_alloc_resource(globals->callback_resource, sizeof(callback_resource));
  memset(callback, 0, sizeof(callback_resource));
  
  callback->condition =  enif_cond_create((char*)"call_fn_wait");
  ErlNifMutex* wait_mutex = enif_mutex_create((char*)"call_fn_mutex");
  callback->env = env;

  // We'll not release it, cos Erlang will think it's finished with as soon as the callback is invoked
  ERL_NIF_TERM resource = enif_make_resource(env, callback);
  ErlNifPid self;
  enif_self(env, &self);
  
  enif_send(env, &globals->owner, NULL, enif_make_tuple4(env,
        enif_make_atom(env, "call_fn"),
        enif_make_pid(env, &self),
        mfa,
        resource)
      );

  // Effectively a spin wait
  enif_cond_wait(callback->condition, wait_mutex);

  // This result has already been copied into our env so we're good
  ERL_NIF_TERM result = callback->result;

  // Now we can clear that resource up
  enif_mutex_destroy(wait_mutex);
  enif_cond_destroy(callback->condition);
  enif_release_resource(callback);

  // Finally, send this back to C# (lol)
  return result;

}

#ifndef __ERLDOTNET_TYPES_H__
#define __ERLDOTNET_TYPES_H__

typedef struct nif_globals_ {
  ErlNifResourceType *fn;
  ErlNifResourceType *hostfxr_resource;
  ErlNifResourceType *bridge_resource;
  ErlNifResourceType *callback_resource;
  ErlNifResourceType *pointer_resource;
  ErlNifPid owner;
} nif_globals;

typedef struct pointer_resource_ {
  void* data;
} pointer_resource;

typedef struct callback_resource_ {
  uint8_t complete;
  ERL_NIF_TERM result;
} callback_resource;

#endif // __ERLDOTNET_TYPES_H__


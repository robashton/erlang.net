#ifndef GUFF_H
#define GUFF_H

#ifdef WINDOWS
#include <Windows.h>

#define STR(s) L ## s
#define CH(c) L ## c
#define DIR_SEPARATOR L'\\'

#else
#include <dlfcn.h>
#include <limits.h>

#define STR(s) s
#define CH(c) c
#define DIR_SEPARATOR '/'
#define MAX_PATH PATH_MAX

#endif

// Forward declarations
void *load_library(const char_t *);
void *get_export(void *, const char *);

#ifdef WINDOWS
void *load_library(const char_t *path)
{
  HMODULE h = ::LoadLibraryW(path);
  assert(h != nullptr);
  return (void*)h;
}
void *get_export(void *h, const char *name)
{
  void *f = ::GetProcAddress((HMODULE)h, name);
  assert(f != nullptr);
  return f;
}
#else
void *load_library(const char_t *path)
{
  void *h = dlopen(path, RTLD_LAZY | RTLD_LOCAL);
  assert(h != nullptr);
  return h;
}
void *get_export(void *h, const char *name)
{
  void *f = dlsym(h, name);
  assert(f != nullptr);
  return f;
}
#endif


#endif

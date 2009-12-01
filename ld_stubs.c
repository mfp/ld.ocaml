#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <string.h>

#include <dlfcn.h>

void *ld_dlsym(void * handle, char * name);
void *ld_dlopen(char * libname);
void ld_dlclose(void * handle);

void *ld_dlopen(char *libname)
{
 return dlopen(libname, RTLD_LAZY);
}

CAMLprim value ld_extract_headers(value file)
{
 CAMLparam1(file);
 CAMLlocal2(ret, sym);
 void *h;

 h = ld_dlopen(String_val(file));
 if(!h) caml_failwith(dlerror());

 sym = (value)dlsym(h, "caml_plugin_header");
 if(!sym) caml_failwith("Couldn't find header");

 ret = caml_alloc_string(string_length(sym));
 memcpy(String_val(ret), String_val(sym), string_length(sym));
 dlclose(h);
 CAMLreturn(ret);
}

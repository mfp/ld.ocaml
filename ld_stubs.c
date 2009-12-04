#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <string.h>

#include <dlfcn.h>

#include <stdlib.h>
#include <stdio.h>
#include <bfd.h>
#include <string.h>

#include <caml/callback.h>


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

static unsigned long int
extract_caml_plugin_offset(char *file)
{
 bfd *abfd;
 asection *sec;
 unsigned long long int data_addr;
 unsigned long long int off;
 char **matching;
 unsigned int ret = -1;

 abfd = bfd_openr(file, "default");
 if (!abfd) return -1;

 if (bfd_check_format_matches (abfd, bfd_object, &matching)) {
     sec = bfd_get_section_by_name(abfd, ".data");
     if (sec) {
         data_addr = sec->lma;
         off = sec->filepos;
         /* printf("LMA %llx, OFFSET %llx\n", data_addr, off); */
         {
              long storage_needed;
              asymbol **symbol_table;
              long number_of_symbols;
              long i;

              storage_needed = bfd_get_dynamic_symtab_upper_bound (abfd);

              if (storage_needed <= 0) goto fail;

              symbol_table = malloc(storage_needed);
              number_of_symbols = bfd_canonicalize_dynamic_symtab (abfd, symbol_table);

              for (i = 0; i < number_of_symbols; i++) {
                  if (!strcmp(symbol_table[i]->name, "caml_plugin_header")) {
                      ret = off + symbol_table[i]->value;
                  }
              }
              free(symbol_table);
         }
     }
 }

fail:
 bfd_close(abfd);
 return ret;
}

CAMLprim value ld_get_caml_plugin_header_offset(value file)
{
 return Val_long(extract_caml_plugin_offset(String_val(file)));
}

char **ld_cmxs_to_load;

CAMLprim
value ld_get_cmxs_to_load(value unit)
{
  CAMLparam0();
  CAMLlocal1(ret);
  ret = caml_copy_string_array((char const **) ld_cmxs_to_load);
  CAMLreturn(ret);
}

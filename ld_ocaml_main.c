#include <stdio.h>
#include <string.h>
#include <caml/callback.h>

extern char **ld_cmxs_to_load;

int main(int argc, char *argv[])
{
  int ncmxs = 0;
  int i, j;
  char **new_argv;

  for (i = 1; i < argc; i++) {
      int len = strlen(argv[i]);
      if(len > 5 && !strcmp(argv[i] + len - 5, ".cmxs"))
          ncmxs++;
      else
          break;
  }

  /* overallocate for convenience, it's ok */
  new_argv = malloc(argc * sizeof(char *));
  ld_cmxs_to_load = malloc((ncmxs + 1) * sizeof(char *));
  for(i = ncmxs, j = 0; i <= argc; ) /* <= to copy final NULL */
      new_argv[j++] = argv[i] ? strdup(argv[i++]) : argv[i++];
  for(i = 1, j = 0; i <= ncmxs; )
      ld_cmxs_to_load[j++] = argv[i++];
  ld_cmxs_to_load[ncmxs] = NULL;

  /* Initialize Caml code */
  caml_main(new_argv);
  return 0;
}


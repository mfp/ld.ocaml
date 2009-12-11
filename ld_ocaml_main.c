#include <stdio.h>
#include <string.h>
#include <caml/callback.h>
#include <unistd.h>

extern char **ld_cmxs_to_load;

/* chop extension:
 *  "foo.exe" => "foo"
 *  "foo/bar.foo/bar" => "foo/bar.foo/bar"
 *  ".bar" => ".bar" */
static char *chop_extension(char *s)
{
  char *pt, *ret;

  ret = strdup(s);

  if(*s == '\0') return ret;

  for(pt = ret + strlen(ret) - 1; pt > ret; pt--) {
      switch(*pt) {
          case '.': *pt = '\0';
          case '/':
          case '\\': return ret;
      }
  }

  return(ret);
}

static char *find_in_path(char *s)
{
 char *path, *tmp, *p1, *p2, *end;
 char file[512];

 path = getenv("PATH");
 if(!path) return NULL;
 tmp = strdup(path);
 end = tmp + strlen(tmp);
 for(p1 = tmp, p2 = index(p1, ':'); p2 < end; ) {
     p2 = p2 ? p2 : end;
     *p2 = '\0';
     sprintf(file, "%s/%s", p1, s);
     if(!access(file, R_OK)) {
         free(tmp);
         return strdup(file);
     }
     p1 = p2 + 1;
     p2 = index(p1, ':');
     p2 = p2 ? p2 : end;
 }
 /* not found */
 free(tmp);
 return NULL;
}

int main(int argc, char *argv[])
{
  int ncmxs = 0;
  int i, j;
  char **new_argv;

  if (strstr(argv[0], "ld_ocaml")) {
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
  } else {
      /* append .cmxs to argv[0] after PATH expansion if needed,
       * load that cmxs, don't modify argv */
      char *cmxs, *tmp;

      tmp = chop_extension(argv[0]);
      cmxs = malloc(strlen(tmp) + 5 + 1);
      sprintf(cmxs, "%s.cmxs", tmp);
      free(tmp);

      new_argv = argv;

      ld_cmxs_to_load = malloc(2 * sizeof(char *));
      ld_cmxs_to_load[0] = strstr(cmxs, "/") ? cmxs : find_in_path(cmxs);
      ld_cmxs_to_load[1] = NULL;
      if(!ld_cmxs_to_load[0]) {
          fprintf(stderr, "Could not find %s in PATH.\n", cmxs);
          exit(1);
      }
  }


  /* Initialize Caml code */
  caml_main(new_argv);
  return 0;
}


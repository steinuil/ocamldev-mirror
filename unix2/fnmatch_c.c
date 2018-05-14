/* Fabrice Le Fessant */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

#define _GNU_SOURCE
#include <fnmatch.h>

#define ML_FNM_NOESCAPE Val_int(0)
#define ML_FNM_PATHNAME Val_int(1)
#define ML_FNM_PERIOD Val_int(2)
#define ML_FNM_FILE_NAME Val_int(3)
#define ML_FNM_LEADING_DIR Val_int(4)
#define ML_FNM_CASEFOLD Val_int(5)

value fnmatch_c(value pattern_v, value string_v, value flags_v)
{
  int flags = 0;
  char *pattern = String_val(pattern_v);
  int res;
  char *string = String_val(string_v);
  
  while(flags_v != Val_unit){
    switch(Field(flags_v, 0)){       
      case ML_FNM_NOESCAPE: flags |= FNM_NOESCAPE; break;
      case ML_FNM_PATHNAME: flags |= FNM_PATHNAME; break;
      case ML_FNM_PERIOD: flags |= FNM_PERIOD; break; 
#ifdef FNM_FILE_NAME
      case ML_FNM_FILE_NAME: flags |= FNM_FILE_NAME; break;
      case ML_FNM_LEADING_DIR: flags |= FNM_LEADING_DIR; break;
      case ML_FNM_CASEFOLD: flags |= FNM_CASEFOLD; break; 
#endif
      
      default:      
      failwith("fnmatch(): unknown flag");
    }
    flags_v = Field(flags_v,1);
  }
  
  res = fnmatch(pattern, string, flags);

  return res ? Val_false : Val_true;
}
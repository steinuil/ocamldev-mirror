/* Fabrice Le Fessant */

#include "alloc.h"
#include "fail.h"
#include "memory.h"

#include <glob.h>

#define ML_GLOB_ONLYDIR       Val_int(0)
#define ML_GLOB_ERR        Val_int(1)
#define ML_GLOB_MARK       Val_int(2)
#define ML_GLOB_NOSORT       Val_int(3)
#define ML_GLOB_NOCHECK       Val_int(4)
#define ML_GLOB_NOESCAPE       Val_int(5)
#define ML_GLOB_PERIOD       Val_int(6)
#define ML_GLOB_BRACE       Val_int(7)
#define ML_GLOB_NOMAGIC       Val_int(8)
#define ML_GLOB_TILDE       Val_int(9)

/* #define ML_GLOB_DOOFS */
/* #define ML_GLOB_ALTDIRFUNC  */
/* #define ML_GLOB_APPEND */

value obj_block_c(int size) /* ML */
{
  value res;
  mlsize_t i;

  if (size == 0) return Atom(0);
  res = alloc(size, 0);
  for (i = 0; i < size; i++) Field(res, i) = Val_long(0);

  return res;
}

value glob_c(value pattern_v, 
  value flags_v)
{
  glob_t pglob;
  int flags = 0;
  char *pattern = String_val(pattern_v);
  int res;
  value result = Val_unit;
  
  while(flags_v != Val_unit){
    switch(Field(flags_v, 0)){
      case ML_GLOB_ONLYDIR: flags |= GLOB_ONLYDIR; break;
      case ML_GLOB_ERR: flags |= GLOB_ERR; break;
      case ML_GLOB_MARK: flags |= GLOB_MARK; break;
      case ML_GLOB_NOSORT: flags |= GLOB_NOSORT; break;
      case ML_GLOB_NOCHECK: flags |= GLOB_NOCHECK; break;
      case ML_GLOB_NOESCAPE: flags |= GLOB_NOESCAPE; break;
      case ML_GLOB_PERIOD: flags |= GLOB_PERIOD; break;
      case ML_GLOB_BRACE: flags |= GLOB_BRACE; break;
      case ML_GLOB_NOMAGIC: flags |= GLOB_NOMAGIC; break;
      case ML_GLOB_TILDE: flags |= GLOB_TILDE; break;
      default:
      failwith("glob(): unknown flag");
    }
    flags_v = Field(flags_v,1);
  }
  
  res = glob(pattern, flags, NULL, &pglob);

  if(res) failwith("glob(): error");

  Begin_root(result);  
  result = obj_block_c(pglob.gl_pathc);
  for(res=0; res < pglob.gl_pathc; res++){
    Modify(&Field(result,res), copy_string(pglob.gl_pathv[res]));
  }
  End_roots();

  globfree(&pglob);

  return result;
}

/* Fabrice Le Fessant */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

#include <wordexp.h>

#define ML_WRDE_NOCMD Val_int(0)
#define ML_WRDE_SHOWERR     Val_int(1)
#define ML_WRDE_UNDEF       Val_int(2)


value obj_block_c(int size) /* ML */
{
  value res;
  mlsize_t i;

  if (size == 0) return Atom(0);
  res = alloc(size, 0);
  for (i = 0; i < size; i++) Field(res, i) = Val_long(0);

  return res;
}

value wordexp_c(value pattern_v, value flags_v)
{
  wordexp_t pwordexp;
  int flags = 0;
  char *pattern = String_val(pattern_v);
  int res;
  value result = Val_unit;
  
  while(flags_v != Val_unit){
    switch(Field(flags_v, 0)){      
      case ML_WRDE_NOCMD: flags |= WRDE_NOCMD; break;
      case ML_WRDE_SHOWERR: flags |= WRDE_SHOWERR; break;
      case ML_WRDE_UNDEF: flags |= WRDE_UNDEF; break;
      
      default:      
      failwith("wordexp(): unknown flag");
    }
    flags_v = Field(flags_v,1);
  }
  
  res = wordexp(pattern, &pwordexp, flags);

  if(res) failwith("wordexp(): error");

  Begin_root(result);  
  result = obj_block_c(pwordexp.we_wordc);
  for(res=0; res < pwordexp.we_wordc; res++){
    Modify(&Field(result,res), copy_string(pwordexp.we_wordv[res]));
  }
  End_roots();

  wordfree(&pwordexp);

  return result;
}

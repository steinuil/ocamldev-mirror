#include "string.h"
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"


value array_sblit(value src_v, value pos1_v, value dest_v, value pos2_v, value len_v)
{
  char *src = (char*) src_v;
  char *dest = (char*) dest_v;
  long pos1 = Long_val(pos1_v);
  long len = Long_val(len_v);
  long pos2 = Long_val(pos2_v);
  if(Tag_val(src_v) == Double_array_tag) {
    src += sizeof(double) * pos1;
    dest += sizeof(double) * pos2;
    len *= sizeof(double);
  } else {
    src += sizeof(value) * pos1;
    dest += sizeof(value) * pos2;
    len *= sizeof(value);
  }

  memmove(dest, src, len);

  return Val_unit;
}

/****************************************************************

                        Integer arrays

*****************************************************************/

value make_ivect(value len, value init) /* ML */
{
  CAMLparam0();
  CAMLlocal1 (res);
  mlsize_t size, wsize, i;
  
  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
  } else {
    if (size > Max_wosize) invalid_argument("Array.make");
    if (size < Max_young_wosize)
      res = alloc_small(size, 0); 
    else 
      res = alloc_shr(size, 0);
    for (i = 0; i < size; i++) Field(res, i) = init;
  }
  CAMLreturn (res);
}

/****************************************************************

                        Float arrays

*****************************************************************/

value make_fvect(value len, value init) /* ML */
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size, wsize, i;
  double d;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
  }
  else if (Is_block(init) && Tag_val(init) == Double_tag) {
    d = Double_val(init);
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) invalid_argument("Array.make");
    res = alloc(wsize, Double_array_tag);
    for (i = 0; i < size; i++) {
      Store_double_field(res, i, d);
    }
  } else {
    if (size > Max_wosize) invalid_argument("Array.make");
    if (size < Max_young_wosize) {
      res = alloc_small(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
    }
    else if (Is_block(init) && Is_young(init)) {
      minor_collection();
      res = alloc_shr(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
      res = check_urgent_gc (res);
    }
    else {
      res = alloc_shr(size, 0);
      for (i = 0; i < size; i++) initialize(&Field(res, i), init);
      res = check_urgent_gc (res);
    }
  }
  CAMLreturn (res);
}


/****************************************************************

                        Pointer arrays

*****************************************************************/

value make_pvect(value len, value init) /* ML */
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size, wsize, i;
  double d;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
  }
  else if (Is_block(init) && Tag_val(init) == Double_tag) {
    d = Double_val(init);
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) invalid_argument("Array.make");
    res = alloc(wsize, Double_array_tag);
    for (i = 0; i < size; i++) {
      Store_double_field(res, i, d);
    }
  } else {
    if (size > Max_wosize) invalid_argument("Array.make");
    if (size < Max_young_wosize) {
      res = alloc_small(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
    }
    else if (Is_block(init) && Is_young(init)) {
      minor_collection();
      res = alloc_shr(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
      res = check_urgent_gc (res);
    }
    else {
      res = alloc_shr(size, 0);
      for (i = 0; i < size; i++) initialize(&Field(res, i), init);
      res = check_urgent_gc (res);
    }
  }
  CAMLreturn (res);
}

value array_checktv(value v)
{
  if(Is_block(v) && Tag_val(v) == Double_tag) return Val_false;
  return Val_true;
}

value array_checkta(value v)
{
  if(Is_block(v) && Tag_val(v) == Double_array_tag) return Val_false;
  return Val_true;
}

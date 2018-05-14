#include "mlvalues.h"
#include "fail.h"
#include "../otherlibs/unix/unixsupport.h"

#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

#define ML_PROT_NONE      Val_int(0)
#define ML_PROT_READ     Val_int(1)
#define ML_PROT_WRITE     Val_int(2)
#define ML_PROT_EXEC     Val_int(3)
  
#define ML_MAP_FIXED     Val_int(0)
#define ML_MAP_SHARED     Val_int(1)
#define ML_MAP_PRIVATE     Val_int(2)
  
#define ML_MS_ASYNC     Val_int(0)
#define ML_MS_SYNC     Val_int(1)
#define ML_MS_INVALIDATE     Val_int(2)

value null_c(value unit)
{ return (value)NULL; }

value malloc_c(value len_v)
{
  void * res = malloc(Long_val(len_v));  

  if(!res) uerror("malloc", Nothing);

  return (value)res;
}

value mm_free_c(value addr_v)
{
  free((void*)addr_v);
  return Val_unit;
}

value mm_add_c(value addr_v, int len_v)
{
  return addr_v+Long_val(len_v);
}

value mm_sub_c(value addr1_v, value addr2_v)
{
  return Val_long(addr1_v - addr2_v);
}

value mm_to_string_c(value addr_v, value string_v, value pos_v, value len_v)
{
  long i;
  long len = Long_val(len_v);
  long pos = Long_val(pos_v);
  char *addr = (char*)addr_v;
  char *string = String_val(string_v);

  bcopy(addr, string+pos, len);

  return Val_unit;
}

value mm_from_string_c(value addr_v, value string_v, value pos_v, value len_v)
{
  long i;
  long len = Long_val(len_v);
  long pos = Long_val(pos_v);
  char *addr = (char*)addr_v;
  char *string = String_val(string_v);

  bcopy(string+pos, addr, len);
  return Val_unit;
}

value mprotect_c(value addr_v, value len_v, value flags_v)
{
  void *addr = (void*) addr_v;
  long len = Long_val(len_v);
  int flags = 0;

  while(flags_v != Val_unit){
    switch(Field(flags_v, 0)){       
      case ML_PROT_NONE: flags |= PROT_NONE; break;
      case ML_PROT_READ: flags |= PROT_READ; break;
      case ML_PROT_WRITE: flags |= PROT_WRITE; break; 
      case ML_PROT_EXEC: flags |= PROT_EXEC; break;
      
      default:      
      failwith("mprotect_c(): unknown flag");
    }
    flags_v = Field(flags_v,1);
  }

  if(mprotect(addr, len, flags)) uerror("mprotect", Nothing);

  return Val_unit;
}

value mmap_c(value addr_v, value len_v, value prots_v, 
  value flags_v, value fd_v, value off_v)
{

  void *addr = (void*) addr_v;
  long len = Long_val(len_v);
  int prots = 0;
  void *res;
  long offset = Long_val(off_v);
  long fd = Long_val(fd_v);
  int flags = 0;

  while(prots_v != Val_unit){
    switch(Field(prots_v, 0)){       
      case ML_PROT_NONE: prots |= PROT_NONE; break;
      case ML_PROT_READ: prots |= PROT_READ; break;
      case ML_PROT_WRITE: prots |= PROT_WRITE; break; 
      case ML_PROT_EXEC: prots |= PROT_EXEC; break;
      
      default:      
      failwith("mmap_c(): unknown prot");
    }
    prots_v = Field(prots_v,1);
  }

  while(flags_v != Val_unit){
    switch(Field(flags_v, 0)){       
      case ML_MAP_FIXED: flags |= MAP_FIXED; break;
      case ML_MAP_SHARED: flags |= MAP_SHARED; break;
      case ML_MAP_PRIVATE: flags |= MAP_PRIVATE; break; 
      
      default:      
      failwith("mmap_c(): unknown flag");
    }
    flags_v = Field(flags_v,1);
  }

  res = mmap(addr, len, prots, flags, fd, offset);
  if(!res)    uerror("mmap", Nothing);

  return (value)res;
}

value mmap_c_byte(value *argv, value argc)
{
  return mmap_c(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value munmap_c(value addr_v, value len_v)
{
  if(munmap((void*)addr_v, Long_val(len_v))) uerror("munmap", Nothing);
  return Val_unit;
}

value msync_c(value addr_v, value len_v, value flags_v)
{
  int flags = 0;

  while(flags_v != Val_unit){
    switch(Field(flags_v, 0)){       
      case ML_MS_ASYNC: flags |= MS_ASYNC; break;
      case ML_MS_SYNC: flags |= MS_SYNC; break;
      case ML_MS_INVALIDATE: flags |= MS_INVALIDATE; break; 
      
      default:      
      failwith("msync_c(): unknown flag");
    }
    flags_v = Field(flags_v,1);
  }

  if(msync((void*) addr_v, Long_val(len_v), flags)) uerror("msync", Nothing);

  return Val_unit;
}

value getpagesize_c(value unit)
{
  return Val_long(getpagesize());
}


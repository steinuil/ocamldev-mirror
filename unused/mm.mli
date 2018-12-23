type addr

type mprot_flags =
  PROT_NONE
| PROT_READ
| PROT_WRITE
| PROT_EXEC
  
type mmap_flags =
  MAP_FIXED
| MAP_SHARED
| MAP_PRIVATE
  
type msync_flags =
  MS_ASYNC
| MS_SYNC
| MS_INVALIDATE
  
(* some operations on addresses *)  
external null: unit -> addr = "null_c"
external malloc: int -> addr = "malloc_c"
external free: addr -> unit = "mm_free_c"
external add: addr -> int -> addr = "mm_add_c"
external sub: addr -> addr -> int = "mm_sub_c"
external to_string: addr -> string -> int -> int = "mm_to_string_c"
external from_string: addr -> string -> int -> int = "mm_from_string_c"

(* system operations on addresses *)
external mprotect: addr -> int -> mprot_flags list -> unit = "mprotect_c"
external mmap: addr -> int -> mprot_flags list -> mmap_flags list -> 
  Unix.file_descr -> int -> addr = "mmap_c_byte" "mmap_c"
external munmap: addr -> int -> unit =  "munmap_c"
external msync: addr -> int -> msync_flags list -> unit = "msync_c"
external getpagesize: unit -> int = "getpagesize_c"

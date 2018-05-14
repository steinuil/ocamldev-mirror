type flags =
| NOESCAPE
| PATHNAME
| PERIOD
| FILE_NAME
| LEADING_DIR
| CASEFOLD
  
external fnmatch : string -> string -> flags list -> bool = "fnmatch_c"
  
type flags =
| NOESCAPE
| PATHNAME
| PERIOD
| FILE_NAME
| LEADING_DIR
| CASEFOLD
  
external fnmatch : string -> string -> flags list -> bool = "fnmatch_c"
(*d [fnmatch pattern string flags] checks whether [string] matches [pattern]
  according to the [fnmatch(3)] function. *)
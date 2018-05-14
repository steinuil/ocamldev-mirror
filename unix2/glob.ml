type flags =
| ONLYDIR
| ERR
| MARK
| NOSORT
| NOCHECK
| NOESCAPE
| PERIOD
| BRACE
| NOMAGIC
| TILDE
  
external glob : string -> flags list -> string array = "glob_c"
  
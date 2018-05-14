type flags =
| NOCMD
| SHOWERR
| UNDEF

external wordexp : string -> flags list -> string array = "wordexp_c"
  
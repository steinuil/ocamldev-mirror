type flags =
| NOCMD
| SHOWERR
| UNDEF

external wordexp : string -> flags list -> string array = "wordexp_c"
(*d [wordexp string flags] splits the string in an array of strings 
(separated by blanks) where shell expansions have been performed. *)
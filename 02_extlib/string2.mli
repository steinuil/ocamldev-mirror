(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

val string_ncmp : string -> string -> int -> bool
(*d compare two strings, looking just the first n characters *)

val search_from : string -> int -> string -> int
(*d [search_from doc pos str] search the first occurence of the string
   [str] in [doc], starting at index [pos] *)

val replace : string -> char -> string -> string
(*d [replace doc c str] replace all the occurences of the char [c] in
   [doc] by the string [str] *)

val split : string -> char -> string list
(*d [split str c] split the string [str] in substrings separated by
the character [c] *)
  
val unsplit : string list -> char -> string
(*d [unsplit str c] reverse operation of [split] *)
  
val convert : 'a -> (Buffer.t -> 'a -> char -> 'a) -> string -> string
  
(*d [convert init f s] iterates on all characters of [s], calling [f] to 
fill a generated buffer using a status value, [init] being the initial
status for the first call to [f]. It returns the content of the buffer. *)
  
val before : string -> int -> string
(*d [before s pos] returns the substring of [s] starting at pos 0 and 
of length [pos]. *)
  
val after : string -> int -> string
(*d [after s pos] returns the end substring of [s] starting at [pos]. *)
  
val cut_at : string -> char -> string * string
(*d [cut_at s c] returns the substrings of [s] before and after the first [c]
  in [s]. The second substring is empty if [s] doesn't contain [c].
The first [c] of [s] is not contained in the two substrings.
*)
  
val check_prefix : string -> string -> bool
(*d [check_prefix s prefix] checks whether [prefix] is a prefix of [s]. *)
  
val upp_initial : string -> string
(*d [upp_initial s] returns a copy of [s] with uppercase first character. *)
  
val subequal : string -> int -> string -> int -> int -> bool
(*d [subequal s1 pos1 s2 pos2 len] checks without allocation whether
the sub-strings of [s1] at [pos1] of [len] and of [s2] at [pos2] of [len]
are equals. *)
  
val subcontains : string -> string -> bool
(*d [subcontains s sub] checks whether [sub] appears in [s]. *)
  
val of_char : char -> string
(*d [of_char c] returns the string containing one [c]. *)
  
val resize : string -> int ->  string
(*d [resize s len] returns a string of length [len] starting with [s]. *)
  
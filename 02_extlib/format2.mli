(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

val redirect_to_string : ('a -> unit) -> 'a -> string
(*d [redirect_to_string f v] applies [f] to [v] and returns the string
filled by a redirection of the standard formatter. *)
  
val formatter_of_out_channel : out_channel -> Format.formatter
(* [formatter_of_out_channel oc] returns a formatter that redirects its
output in oc *)
val null_formatter : Format.formatter
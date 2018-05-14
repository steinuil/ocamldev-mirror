(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

val logp : bool ref
val log_name : string ref
val printf : ('a -> 'b, out_channel, unit) format -> 'a -> unit
val exn : (string -> 'a, out_channel, unit) format -> exn -> unit

val catch :
  (string -> 'a, out_channel, unit) format -> (unit -> unit) -> unit
val watch : (string -> 'a, out_channel, unit) format -> (unit -> 'b) -> 'b

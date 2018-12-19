(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

val logp : bool ref
val log_name : string ref
val printf : ('a -> unit, out_channel, unit) format -> 'a -> unit
val exn : (string -> unit, out_channel, unit) format -> exn -> unit

val catch :
  (string -> unit, out_channel, unit) format -> (unit -> unit) -> unit
val watch : (string -> unit, out_channel, unit) format -> (unit -> 'b) -> 'b

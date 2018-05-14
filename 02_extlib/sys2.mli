(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

val copy_file : in_channel -> out_channel -> unit
(*d [copy_file ic oc] copies channel [ic] to channel [oc] until the end 
of the file. *)
  
val copy_file_chunk : in_channel -> out_channel -> int -> unit
(*d [copy_file_chunk ic oc len] copies [len] bytes from channel [ic] to 
  channel [oc],  and raises End_of_file if the end of the file is reached. *)
  
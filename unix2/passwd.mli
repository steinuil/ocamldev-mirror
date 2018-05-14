(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

(* The "HOME" environment variable *)
val home : string
  
(* an entry from the /etc/passwd file *)
type entry = {
    login : string;
    uid : int;
    gid : int;
    name : string;
    homedir : string;
    shell : string;
  } 

(* Read all the entries from the /etc/passwd file. *)
val get_entries : unit -> entry list

(* Clear the cache of entries from the /etc/passwd file *)
val clear_entries : unit -> unit
  
(* convert a filename to a string, replacing homedir/ by ~user, based
  on the entries read from the /etc/passwd file. *)
val filename_to_string : string -> string

(* convert a string to a filename, replacing ~user by homedir/, based
  on the entries read from the /etc/passwd file. *)
val string_to_filename : string -> string
  
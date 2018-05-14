(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)


type t
type 'a var

val new_var : unit -> 'a var
val empty : unit -> t
val get : t -> 'a var -> 'a
val set : t -> 'a var -> 'a -> unit
val sget : t -> 'a var -> 'a -> 'a
val remove : t -> 'a var -> unit
  
type env
val copy_env : env -> env
val empty_env : unit -> env
val get_env : env -> 'a var -> 'a
val set_env : env -> 'a var -> 'a -> unit
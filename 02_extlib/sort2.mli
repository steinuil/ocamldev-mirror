(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

(* $Id *)

(*c The [Topo] module can be used to sort elements according to a 
partial order. *)

module Topo: sig

    type 'a partial_order
    
    exception Cyclic
    
    val create : unit -> 'a partial_order
(*d [create ()] creates an empty partial order *)
      
    val add : 'a partial_order -> 'a -> unit
(*d [add t a] adds the element [a] to the partial order [t] *)
      
    val inf : 'a partial_order -> 'a -> 'a -> unit
(*d [inf t a b] adds the relation  [a] &inf; [b] to the partial order 
  [t]. It raises [Cyclic] is the partial order contains a cycle. *)

    val less : 'a partial_order -> 'a -> 'a -> bool
(*d [less t a b] returns [true] is [a] is inferior to [b] according to
the partial order [t]. It raises [Cyclic] is the partial order contains
  a cycle.  *)
      
    val list : 'a partial_order -> 'a list -> 'a list
(*d [list t list] sorts [list] according to the partial order [t].
It raises [Cyclic] is the partial order contains a cycle.   *)
      
    val cycle : 'a partial_order -> 'a list
(*d [cycle t] returns a list of elements which have a cyclic relation
in the partial order [t] *)
      
    val ordered : 'a partial_order -> bool
(*d [ordered t] returns [true] is the partial order [t] has no cycles *)
      
  end
  
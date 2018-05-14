(* (C)        Jean-François Monin, 1999            *)
(* Centre National d'Etudes des Télécommunications *)
(* $Id: splaytbl.mli,v 1.1 2001/04/15 18:57:14 lefessan Exp $ *)
module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

(* Functorial interface *)
module type S =
  sig
    type key
    type 'a t
    val create : unit -> 'a t
    val clear : 'a t -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val mem : 'a t -> key -> bool
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val elements : 'a t -> (key * 'a) list
    val is_empty : 'a t -> bool
    val min_elt : 'a t -> key * 'a
    val max_elt : 'a t -> key * 'a
    val cardinal : 'a t -> int
  end

module Make(Ord: OrderedType):
    (S with type key = Ord.t)
        (* Functor building an implementation of the tree structure
           given a totally ordered type. *)

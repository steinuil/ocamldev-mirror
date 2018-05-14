(* (C)        Jean-François Monin, 1999            *)
(* Centre National d'Etudes des Télécommunications *)
(* $Id: splay.mli,v 1.1 2001/04/15 18:57:14 lefessan Exp $ *)
(* NOT THREAD SAFE : env needs a mutex *)
(* WARNING : compare must not raise any exception *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

          (* The input signature of the functor [Splay.Make].
             [key] is the type of keys in the tree.
             [compare] is a total ordering function over the keys.
             This is a two-argument function [f] such that
             [f e1 e2] is zero if the elements [e1] and [e2] are equal,
             [f e1 e2] is strictly negative if [e1] is smaller than [e2],
             and [f e1 e2] is strictly positive if [e1] is greater than [e2].
             Example: a suitable ordering function is
             the generic structural comparison function [compare].
	  *)

module type S =
  sig
    type key
          (* The type of keys in the tree. *)
    type 'a t
          (* The type of trees. *)
    exception Already_there
    val print : (key -> unit) -> ('a -> unit) -> 'a t -> unit
	(* prints a tree *)
    val create: unit -> 'a t
          (* The empty tree. *)
    val clear:  'a t -> unit
          (* clears the tree. *)
    val min_elt: 'a t -> key * 'a
        (* Return the smallest element of the given tree
           (with respect to the [Ord.compare] ordering), or raise
           [Not_found] if the tree is empty. *)
    val max_elt: 'a t -> key * 'a
        (* Same as [min_elt], but returns the largest element of the
           given tree. *)
    val find: 'a t -> key -> 'a
        (* [find s x] is an element y of [s] such that [compare x y = 0].
	   If [x] was not in [s], [Not_found] is raised. *)
    val mem: 'a t -> key -> bool
        (* [mem s x] is true if there is an element y of [s]
	   such that [compare x y = 0]. *)
    val add: 'a t -> key -> 'a -> unit
        (* [add s k x] adds the element [k,x] to the tree [s],
           If [x] was already in [s], [Already_there] is raised. *)
    val remove: 'a t -> key -> unit
        (* [remove s x] returns a tree containing all elements of [s],
           except [y] such that [compare x y = 0].
	   If [x] was not in [s], nothing is done (excepted rearrangement) *)
    val set: 'a t -> key -> 'a -> unit
        (* [set s k x] replaces the element [k,_] of the tree [s] by [k,x]
           If [x] was not already in [s], [k,x] is simply added.          *)
    val sub: 'a t -> key -> key -> 'a t
	(* [sub s c1 c2] returns a fresh tree of consecutive elements of [s] *)
        (* such that their key [c] satisfies  [c1 <= c < c2] *)
    val from: 'a t -> key -> 'a t
	(* [froù s c1] returns a fresh tree of consecutive elements of [s] *)
        (* such that their key [c] satisfies  [c1 <= c] *)
    val floor: 'a t -> key -> key * 'a
	(* [floor s c] returns the greatest element with [key <= c] *)
    val ceil: 'a t -> key -> key * 'a
	(* [ceil s c] returns the smallest element with [key >= c] *)
    val prev: 'a t -> key -> key * 'a
	(* [prev s c] returns the greatest element with [key < c] *)
    val next: 'a t -> key -> key * 'a
	(* [next s c] returns the smallest element with [key > c] *)

    (* The following functions don't have any side effect on their tree arg *)

    val copy: 'a t -> 'a t
          (* creates a fresh copy *)
    val iter: (key -> 'a -> unit) -> 'a t -> unit
        (* [iter s f] applies [f] in increasing order to all elements of [s] *)
    val fold_left: ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
        (* [fold_left f a s] computes [f (...(f (f a c1 x1) c2 x2)...) cn xn],
           where [c1,x1... cn,xn] are the elements of [s] in increasing order. *)
    val fold_right: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        (* [fold_right f s b] computes [f c1 x1 (... (f c2 x2 (f cn xn b))...)],
           where [c1,x1... cn,xn] are the elements of [s] in increasing order. *)
    val cardinal: 'a t -> int
        (* Return the number of elements of a tree. *)
    val is_empty: 'a t -> bool
        (* Test whether a tree is empty or not. *)
    val to_list: 'a t -> (key * 'a) list
        (* Returns the list of all elements of the given tree
           in increasing order wrt [Ord.compare] *)
    val to_stream : 'a t -> (key * 'a) Stream.t
        (* Returns the stream of all elements of the given tree
           in increasing order wrt [Ord.compare] *)
    val filter: (key -> 'a -> bool) -> 'a t ->  'a t
        (* [filter p s] returns a tree of all elements [c,v] of [s]
	   such that [p c v], in increasing order wrt [Ord.compare] *)
    val map: (key -> 'a -> 'b) -> 'a t ->  'b t
        (* [map f s] returns an tree to [s] where values v 
	   are replaced with [f c v] *)

  end

module Make(Ord: OrderedType):
    (S with type key = Ord.t)
        (* Functor building an implementation of the tree structure
           given a totally ordered type. *)

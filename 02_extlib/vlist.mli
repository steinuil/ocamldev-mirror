(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: vlist.mli,v 1.3 2001/09/09 22:43:43 lefessan Exp $ *)

(* Module [Vlist]: list operations using vectors instead of list.
Sharing is not preserved in list tails. However, the vlist take much less
  memory than standard lists.
  *)

(* Some functions are flagged as not tail-recursive.  A tail-recursive
   function uses constant stack space, while a non-tail-recursive function
   uses stack space proportional to the length of its list argument, which
   can be a problem with very long lists.  When the function takes several
   list arguments, an approximate formula giving stack usage (in some
   unspecified constant unit) is shown in parentheses.

   The above considerations can usually be ignored if your lists are not
   longer than about 10000 elements.
*)

type 'a t

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
  
val length : 'a t -> int
        (* Return the length (number of elements) of the given t. *)
val hd : 'a t -> 'a
        (* Return the first element of the given t. Raise
           [Failure "hd"] if the t is empty. *)
val tl : 'a t -> 'a t
        (* Return the given t without its first element. Raise
           [Failure "tl"] if the t is empty. *)
val nth : 'a t -> int -> 'a
        (* Return the n-th element of the given t.
           The first element (head of the t) is at position 0.
           Raise [Failure "nth"] if the t is too short. *)
val rev : 'a t -> 'a t
        (* List reversal. *)
val append : 'a t -> 'a t -> 'a t
        (* Catenate two ts.  Same function as the infix operator [@].
           Not tail-recursive (length of the first argument).  The [@]
           operator is not tail-recursive either. *)
val rev_append : 'a t -> 'a t -> 'a t
        (* [List.rev_append l1 l2] reverses [l1] and catenates it to [l2].
           This is equivalent to [List.rev l1 @ l2], but [rev_append] is
           tail-recursive and more efficient. *)
val concat  : 'a t t -> 'a t
val flatten : 'a t t -> 'a t
        (* Catenate (flatten) a t of ts.  Not tail-recursive
           (length of the argument + length of the longest sub-list). *)

(** Iterators *)

val iter : ('a -> unit) -> 'a t -> unit
        (* [List.iter f [a1; ...; an]] applies function [f] in turn to
           [a1; ...; an]. It is equivalent to
           [begin f a1; f a2; ...; f an; () end]. *)
val map : ('a -> 'b) -> 'a t -> 'b t
        (* [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
           and builds the t [[f a1; ...; f an]]
           with the results returned by [f].  Not tail-recursive. *)
val rev_map : ('a -> 'b) -> 'a t -> 'b t
        (* [List.rev_map f l] gives the same result as
           [List.rev (List.map f l)], but is tail-recursive and
more efficient. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
        (* [List.fold_left f a [b1; ...; bn]] is
           [f (... (f (f a b1) b2) ...) bn]. *)
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        (* [List.fold_right f [a1; ...; an] b] is
           [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)

(** Iterators on two ts *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
        (* [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
           [f a1 b1; ...; f an bn].
           Raise [Invalid_argument] if the two ts have
           different lengths. *)
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
        (* [List.map2 f [a1; ...; an] [b1; ...; bn]] is
           [[f a1 b1; ...; f an bn]].
           Raise [Invalid_argument] if the two ts have
different lengths.  Not tail-recursive. *)
  
val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
        (* [List.rev_map2 f l] gives the same result as
           [List.rev (List.map2 f l)], but is tail-recursive and
           more efficient. *)
val fold_left2 :
        ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
        (* [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
           [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
           Raise [Invalid_argument] if the two ts have
           different lengths. *)
val fold_right2 :
        ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
        (* [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
           [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
           Raise [Invalid_argument] if the two ts have
           different lengths.  Not tail-recursive. *)

(** List scanning *)

val for_all : ('a -> bool) -> 'a t -> bool
        (* [for_all p [a1; ...; an]] checks if all elements of the t
           satisfy the predicate [p]. That is, it returns
           [(p a1) && (p a2) && ... && (p an)]. *)
val exists : ('a -> bool) -> 'a t -> bool
        (* [exists p [a1; ...; an]] checks if at least one element of
           the t satisfies the predicate [p]. That is, it returns
[(p a1) || (p a2) || ... || (p an)]. *)
  
  (*
val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
        (* Same as [for_all] and [exists], but for a two-argument predicate.
           Raise [Invalid_argument] if the two ts have
           different lengths. *)
val mem : 'a -> 'a t -> bool
        (* [mem a l] is true if and only if [a] is equal
           to an element of [l]. *)
val memq : 'a -> 'a t -> bool
        (* Same as [mem], but uses physical equality instead of structural
           equality to compare t elements. *)

(** List searching *)

val find : ('a -> bool) -> 'a t -> 'a
        (* [find p l] returns the first element of the t [l]
           that satisfies the predicate [p].
           Raise [Not_found] if there is no value that satisfies [p] in the
           t [l]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
val find_all : ('a -> bool) -> 'a t -> 'a t
        (* [filter p l] returns all the elements of the t [l]
           that satisfy the predicate [p].  The order of the elements
           in the input t is preserved.  [find_all] is another name
           for [filter]. *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
        (* [partition p l] returns a pair of ts [(l1, l2)], where
           [l1] is the t of all the elements of [l] that
           satisfy the predicate [p], and [l2] is the t of all the
           elements of [l] that do not satisfy [p].
           The order of the elements in the input t is preserved. *)

(** Association ts *)

val assoc : 'a -> ('a * 'b) t -> 'b
        (* [assoc a l] returns the value associated with key [a] in the t of
           pairs [l]. That is,
             [assoc a [ ...; (a,b); ...] = b]
           if [(a,b)] is the leftmost binding of [a] in t [l].
           Raise [Not_found] if there is no value associated with [a] in the
           t [l]. *)
val assq : 'a -> ('a * 'b) t -> 'b
        (* Same as [assoc], but uses physical equality instead of structural
           equality to compare keys. *)

val mem_assoc : 'a -> ('a * 'b) t -> bool
        (* Same as [assoc], but simply return true if a binding exists,
           and false if no bindings exist for the given key. *)
val mem_assq : 'a -> ('a * 'b) t -> bool
        (* Same as [mem_assoc], but uses physical equality instead of
           structural equality to compare keys. *)

val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t
        (* [remove_assoc a l] returns the t of
           pairs [l] without the first pair with key [a], if any.
           Not tail-recursive. *)

val remove_assq : 'a -> ('a * 'b) t -> ('a * 'b) t
        (* Same as [remove_assq], but uses physical equality instead
           of structural equality to compare keys.  Not tail-recursive. *)

(** Lists of pairs *)

val split : ('a * 'b) t -> 'a t * 'b t
        (* Transform a t of pairs into a pair of ts:
           [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
           Not tail-recursive.
        *)
val combine : 'a t -> 'b t -> ('a * 'b) t
        (* Transform a pair of ts into a t of pairs:
           [combine ([a1; ...; an], [b1; ...; bn])] is
              [[(a1,b1); ...; (an,bn)]].
           Raise [Invalid_argument] if the two ts
           have different lengths.  Not tail-recursive. *)

(** Sorting *)
val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t;;
        (* Sort a t in increasing order according to a comparison
           function.  The comparison function must return 0 if it arguments
           compare as equal, a positive integer if the first is greater,
           and a negative integer if the first is smaller.  For example,
           the [compare] function is a suitable comparison function.
           The resulting t is sorted in increasing order.
           [List.sort] is guaranteed to run in constant heap space
           (in addition to the size of the result t) and logarithmic
           stack space.

           The current implementation uses Merge Sort and is the same as
           [List.stable_sort].
        *)
val stable_sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t;;
        (* Same as [List.sort], but the sorting algorithm is stable.

           The current implementation is Merge Sort. It runs in constant
           heap space and logarithmic stack space.
        *)
*)
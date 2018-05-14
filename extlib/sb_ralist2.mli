
(*
   Skew Binary Random-Access List - random access to elements in O(log n)

   Copyright (c) 1999 Pierpaolo Bernardi
                      bernardp@CLI.DI.Unipi.IT

   Minor changes by Markus Mottl
                    mottl@miss.wu-wien.ac.at

   This source code is free software; you can redistribute it and/or
   modify it without any restrictions. It is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY.

   For a theoretical discussion of this data structure see
   chapter 9.3.1 in:

       Purely Functional Data Structures
       Chris Okasaki
       Cambridge University Press, 1998
       Copyright (c) 1998 Cambridge University Press
*)

(* most functions in this module are (except for performance) comparable
   to those found in the "List"-module of the OCAML standard library *)

type 'a t

val empty : 'a t
val cons  : 'a -> 'a t -> 'a t
val hd    : 'a t -> 'a
val tl    : 'a t -> 'a t

val is_empty : 'a t -> bool

val nth    : 'a t -> int -> 'a
val update : 'a t -> int -> 'a -> 'a t

val length  : 'a t -> int
val create  : 'a -> int -> 'a t
val drop    : 'a t -> int -> 'a t

val rev_append : 'a t -> 'a t -> 'a t
val rev : 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t t -> 'a t
val flatten : 'a t t -> 'a t
val to_list : 'a t -> 'a list
val from_list : 'a list -> 'a t
val to_array : 'a t -> 'a array
val from_array : 'a array -> 'a t
val iota : int -> int t
        (* [iota n] generates list of integers from 0 to n-1 *)
val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_left1 : ('a -> 'a -> 'a) -> 'a t -> 'a
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
val iter : ('a -> unit) -> 'a t -> unit
val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
val for_all : ('a -> bool) -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val mem : 'a -> 'a t -> bool
val memq : 'a -> 'a t -> bool
val assoc : 'a -> ('a * 'b) t -> 'b
val mem_assoc : 'a -> ('a * 'b) t -> bool
val assq : 'a -> ('a * 'b) t -> 'b
val split : ('a * 'b) t -> 'a t * 'b t
val combine : 'a t -> 'b t -> ('a * 'b) t

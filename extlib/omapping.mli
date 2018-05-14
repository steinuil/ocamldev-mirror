(* $Id: omapping.mli,v 1.1 2001/04/15 20:12:10 lefessan Exp $ *)

(* An embryo of class hierarchy for mappings.
   4 implementations of Omapping.c are provided:

    * Omapping.alist: association lists
    * Omap.c, Omap.f: BDD-based maps, imperative and functional versions
    * Ohashtbl.c: Hash tables

   Omapping.alist and Omap.f support the stronger functional interface
   Omapping.f.
   We have also Ohashtbl.c <: Omap.c *)

class type ['a,'b] c = object
  method add : key:'a -> data:'b -> unit
  method find : 'a -> 'b
  method iter : f:(key:'a -> data:'b -> unit) -> unit
end

class type ['a,'b] f = object ('c)
  method add : key:'a -> data:'b -> 'c
  method find : 'a -> 'b
  method iter : f:(key:'a -> data:'b -> unit) -> unit
end

class ['a,'b] alist : ?eq:[`logical|`physical] -> ('a * 'b) list -> ['a,'b] f
      (* A class wrapper for association lists.
	 If [eq] is [`physical] then use List.assq for find,
	 otherwise use List.assoc. *)

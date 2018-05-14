(* $Id: omap.mli,v 1.1 2001/04/15 20:12:09 lefessan Exp $ *)

class ['a, 'b] c : ?compare:('a -> 'a -> int) -> ('a * 'b) list -> object
      (* [new c bindings :compare] creates a new map using
      	 compare as comparison function, and initializes it
	 with the given bindings.
	 [new c bindings] uses Pervasives.compare.
	 This map is imperative, and behaves exactly like
	 Hashtbl.c, except that previous old bindings for the
      	 same key are not kept *)
  method clear : unit
  method add : key:'a -> data:'b -> unit
  method find : 'a -> 'b
  method mem : 'a -> bool
  method remove : 'a -> unit
  method iter : f:(key:'a -> data:'b -> unit) -> unit
end

class ['a, 'b] f : ?compare:('a -> 'a -> int) -> ('a * 'b) list -> 
object ('c)
      (* functional version of Omap.c *)
  method add : key:'a -> data:'b -> 'c
  method find : 'a -> 'b
  method mem : 'a -> bool
  method remove : 'a -> 'c
  method iter : f:(key:'a -> data:'b -> unit) -> unit
end

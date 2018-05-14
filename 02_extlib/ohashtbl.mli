(* $Id: ohashtbl.mli,v 1.1 2001/04/15 20:12:09 lefessan Exp $ *)

class ['a, 'b] c :
    ?eq:('a -> 'a -> bool) -> ?hash:('a -> int) -> int ->
object
      (* [new c :size :eq :hash] creates a new hash table
	 of given initial size, hash function, and using equality
	 [eq] on keys.
	 [eq] and [hash] default to Pervasives.(=) (logical equality)
	 and Hashtbl.hash. [hash] shall be such that two keys equal
	 by [eq] have the same image by [hash] *)
  method clear : unit
  method add : key:'a -> data:'b -> unit
  method find : 'a -> 'b
  method find_all : 'a -> 'b list
  method remove : 'a -> unit
  method mem : 'a -> bool
  method iter : f:(key:'a -> data:'b -> unit) -> unit
end

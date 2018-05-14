(* $Id: omapping.ml,v 1.2 2001/09/09 22:43:42 lefessan Exp $ *)

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

class ['a,'b] alist ?eq l = object (_ : ('a,'b) #f)
  val assoc =
    match eq with
      Some (`physical : [`physical|`logical]) -> List.assq
    | _ -> List.assoc
  val l = l
  method add ~key ~data = {< l = (key,data)::l >}
  method find key = assoc key l
  method iter ~f =
    List.iter (fun (key,data) -> f ~key ~data)  l
end

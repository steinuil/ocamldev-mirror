(* $Id: ostack.ml,v 1.1 2001/04/15 20:12:11 lefessan Exp $ *)

exception Empty

class ['a] c () = object
  val mutable s : 'a list = []
  method push x = s <- x :: s
  method pop =
    match s with x::s' -> s <- s'; x
    | [] -> raise Empty
  method top =
    match s with x::_ -> x
    | [] -> raise Empty
  method clear = s <- []
  method length = List.length s
  method iter f = List.iter f s
  method contents = s
end

(* $Id: ostack.mli,v 1.2 2001/09/09 22:43:42 lefessan Exp $ *)

exception Empty

class ['a] c : unit -> object
  val mutable s : 'a list
  method push : 'a -> unit
  method pop : 'a
  method top : 'a
  method clear : unit
  method length : int
  method iter : ('a -> unit) -> unit
  method contents : 'a list
end

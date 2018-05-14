(* $Id: oqueue.mli,v 1.1 2001/04/15 20:12:10 lefessan Exp $ *)

class ['a] c : unit -> object
  val q : 'a Queue.t
  method add : 'a -> unit
  method take : 'a
  method peek : 'a
  method clear : unit
  method length : int
  method iter : f:('a -> unit) -> unit
end

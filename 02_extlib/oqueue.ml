(* $Id: oqueue.ml,v 1.1 2001/04/15 20:12:10 lefessan Exp $ *)

open Queue

class ['a] c () = object
  val q = create ()
  method add (x : 'a) = add x q
  method take = take q
  method peek = peek q
  method clear = clear q
  method length = length q
  method iter  ~f = iter f q
end

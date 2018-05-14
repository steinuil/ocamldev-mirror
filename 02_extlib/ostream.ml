(* $Id: ostream.ml,v 1.2 2001/09/09 22:43:42 lefessan Exp $ *)

open Stream

class ['a] c s = object
  val mutable s = (s : 'a Stream.t)
  method out = s
  method iter ~f  = iter f s
  method next = next s
  method empty = empty s
  method peek =  peek s
  method junk = junk s
  method count = count s
end

let from f = new c (from f)
and of_list l = new c (of_list l)
and of_string s = new c (of_string s)
and of_channel ic = new c (of_channel ic)

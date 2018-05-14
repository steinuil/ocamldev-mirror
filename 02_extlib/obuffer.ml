(* $Id: obuffer.ml,v 1.2 2001/09/09 22:43:41 lefessan Exp $ *)

open Buffer

class c size = object
  val b = create size
  method contents = contents b
  method length = length b
  method clear = clear b
  method reset = reset b
  method add_char = add_char b
  method add_string = add_string b
  method add_substring = add_substring b
  method add_buffer src = add_buffer b src
  method add_channel = add_channel b
  method output oc = output_buffer oc b
end

(* $Id: obuffer.mli,v 1.2 2001/09/09 22:43:41 lefessan Exp $ *)

class c : int -> object
  val b : Buffer.t
  method add_buffer : Buffer.t -> unit
  method add_channel : in_channel -> int -> unit
  method add_char : char -> unit
  method add_string : string -> unit
  method add_substring : string -> int -> int -> unit
  method clear : unit
  method contents : string
  method length : int
  method output : out_channel -> unit
  method reset : unit
end

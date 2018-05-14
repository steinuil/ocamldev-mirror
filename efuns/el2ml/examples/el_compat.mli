
type t =
  String of string
| Integer of int
| Float of float
| Quoted of string
| Nil
| Vector of t list
| Pair of t * t
| List of t list
| Char of char
  
val get_var : string -> t
val to_boolean : t -> bool
val to_function : t -> t list -> t
val defvar : string -> t -> string -> unit
val defun : string -> (t list -> t) -> t
val make_variable_buffer_local : string -> unit
val set_arg : string -> int -> t list -> t
  
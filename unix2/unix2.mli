(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

val list_directory : string -> string list
(*d [list_directory dirname] returns a list of the files included in
the directory [dirname]. The list doesn't contain the "." and ".." 
entries *)

val is_directory : string -> bool
(*d [is_directory filename] checks whether [filename] is a directory or not *)

val is_link : string -> bool
(*d [is_directory filename] checks whether [filename] is a link or not *)
  
val file_size : string -> int
(*d [file_size filename] returns the size of [filename] *)

val safe_mkdir : string -> unit
(*d [safe_mkdir dirname] recursively creates the directory [dirname] and
its parent directories. *)
  
val safe_move : string -> string -> unit
(*d [safe_move src_name dest_name] renames the file [src_name] to 
[dest_name], by creating necessary parent directories if needed. *)
  
val safe_copy : string -> string -> unit
(*d [safe_copy src_name dest_name] copies the file [src_name] to 
[dest_name], by creating necessary parent directories if needed. *)
  
val find : string -> (string -> bool) -> (string -> unit) -> unit
(*d [find dirname predicate action] recursively scans the directory
[dirname], applying [action] to files if [predicate] is true for these files.
[predicate] and [action] take name of the files relatively to [dirname] *)
  
    
type date_format = 
  Second
| Minute
| Hour
| Day
| WeekDay
| Month
| Year
| Space
| Colon
  
val months : string array
val days : string array
val string_of_date : date_format list -> Unix.tm -> string

val getdomainname: unit -> string
(* [getdomainname ()] returns the domainame of the current host. *)
  
val fork : ('a -> unit) -> 'a -> int
val init : unit -> unit
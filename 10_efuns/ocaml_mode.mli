(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)



val lexer_start : Text.position ref
val position : Lexing.lexbuf -> int * int
type lexer_error =
  | Illegal_character
  | Unterminated_comment
  | Unterminated_string
exception Error of int * int * lexer_error
val keyword_color : string Options.option_record
val string_color : string Options.option_record
val comment_color : string Options.option_record
val upper_color : string Options.option_record
val keyword_font : string Options.option_record
val string_font : string Options.option_record
val comment_font : string Options.option_record
val upper_font : string Options.option_record
val ocaml_color_region :
  Efuns.location -> Efuns.buffer -> Text.point -> Text.point -> unit
val ocaml_color_buffer : Efuns.buffer -> unit
val ocaml_color : Efuns.frame -> unit
type indentations = (int * Text.position list) list
val print_indentations : (int * int list) list -> unit
val pop_to : 'a -> ('a * int) list -> ('a * int) list * int
val fix : 'a -> 'b list -> ('a * 'b list) list -> ('a * 'b list) list
val pop_indentation : ('a * 'b list) list -> 'a * 'b * ('a * 'b list) list
val get_indentations : 'a -> Lexing.lexbuf -> (int * Text.position list) list
val print_exc : exn -> string -> unit
val compute_indentations :
  Efuns.buffer -> Text.point -> Text.point -> (int * Text.position list) list
val find_phrase_start : Efuns.buffer -> Text.point -> unit
val indent_between_points : Efuns.buffer -> Text.point -> Text.point -> unit
val indent_phrase : Efuns.frame -> unit
val indent_region : Efuns.frame -> unit
val indent_buffer : Efuns.frame -> unit
val insert_and_return : Efuns.frame -> unit
val indent_current_line : Efuns.frame -> unit
val split1 : string -> char -> string list
val parse_name : string -> string list
val find_long_word : Efuns.buffer -> Text.point -> string
val module_name : string -> string
val ocaml_find_error : Text.t -> Text.point -> Compil.error
val c_c : Efuns.key
val install : Efuns.buffer -> unit
val ocaml_path : string list Options.option_record
val find_env:  Efuns.buffer -> Text.point -> string list
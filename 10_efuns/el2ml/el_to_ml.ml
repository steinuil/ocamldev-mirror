open El_lexer

type t =
| Defun
| Defvar
| Lambda
| If
| Cond
| While
| Save_excursion
| Let
| Let_star
| Interactive
| Progn
| Unwind_protect
| Defconst
| Or 
| And
  
(*
defun
defvar
lambda
if
cond
while
save-excursion
let
let*
interactive
progn
unwind-protect
defconst
or 
and
*)
  
let functions = ref 0
  
let spaces n =
  for i = 0 to n do print_char ' '; done
  
let rec print n v =
  spaces n;
  match v with
    IDENT string -> Printf.printf "IDENT %s" string; print_newline ();
  | COMMENT string -> 
(* Printf.printf "COMMENT [%s]" string; print_newline (); *)
      ()
  | STRING string -> Printf.printf "STRING [%s]" string; print_newline ();
  | LIST list -> 
      Printf.printf "LIST [["; print_newline ();
      print_list (n+2) list;
      spaces n; Printf.printf "]]"; print_newline ();
  | LBRACE -> Printf.printf "{"; print_newline ();
  | RBRACE -> Printf.printf "}"; print_newline ();
  | VECTOR list -> 
      Printf.printf "VECTOR ["; print_newline ();
      print_list (n+2) list;
      spaces n;
      Printf.printf "]"; print_newline ();
  | FLOAT string -> Printf.printf "FLOAT %s" string; print_newline ();
  | INTEGER string -> Printf.printf "INTEGER %s" string; print_newline ();
  | DOT -> Printf.printf "."; print_newline ();
  | QUOTE token -> Printf.printf "'"; print (n+2) token; print_newline ();
  | COMMA -> Printf.printf ","; print_newline ();
  | EOF -> Printf.printf "EOF"; print_newline ();
  | DEFVAR -> Printf.printf "DEFVAR"; print_newline ();
  | DEFUN -> Printf.printf "DEFUN"; print_newline ();
  | CHAR char  -> Printf.printf "CHAR [%c]" char; print_newline ()      

and print_list n list =
  List.iter (print n) list

let ml_of_el_filename filename =
  String2.convert () (fun b v c ->
      match c with
        '-' -> Buffer.add_char b '_'
      | _ -> Buffer.add_char b c) 
  (Filename.chop_suffix filename ".el")

          
let transl_ident ident =
  let ident = 
  String2.convert () (fun b v c ->
      match c with
        '-' -> Buffer.add_string b "_minus_"
      | ':' -> Buffer.add_string b "_semi_"
      | '@' -> Buffer.add_string b "_at_"
      | '=' -> Buffer.add_string b "_equal_"
      | '<' -> Buffer.add_string b "_inf_"
      | '>' -> Buffer.add_string b "_sup_"
      | '|' -> Buffer.add_string b "_bar_"
      | '+' -> Buffer.add_string b "_plus_"
      | '*' -> Buffer.add_string b "_star_"
      | 'A' .. 'Z' ->
          Buffer.add_char b (Char.lowercase c)
      | _ -> Buffer.add_char b c) ident
  in
  "el_" ^ ident
  
let rec transl_expr oc value =
  match value with
    IDENT n -> 
      Printf.fprintf oc "(get_var \"%s\")" (transl_ident n)
  | STRING s ->
      Printf.fprintf oc "(String \"%s\")" (String.escaped s)
  | INTEGER i ->
      Printf.fprintf oc "(Integer %s)" i
  | FLOAT f ->
      Printf.fprintf oc "(Float %s)" f
  | LIST [] ->
      Printf.fprintf oc "Nil";
  | CHAR '\'' ->
      Printf.fprintf oc "(Char '\\'')"
  | CHAR c ->
      let s = String.make 1 c in
      Printf.fprintf oc "(Char '%s')" (String.escaped s)
  | QUOTE (LIST [e1; DOT; e2]) ->
      Printf.fprintf oc "(Pair (";
      transl_expr oc e1;
      Printf.fprintf oc " , ";
      transl_expr oc e2;
      Printf.fprintf oc "))\n"
  | LIST [e1; DOT; e2] ->
      Printf.fprintf oc "(Pair (";
      transl_expr oc e1;
      Printf.fprintf oc " , ";
      transl_expr oc e2;
      Printf.fprintf oc "))\n"
  | LIST list ->
      Printf.fprintf oc "(\n";
      transl_list_expr oc list;
      Printf.fprintf oc ")"
  | QUOTE (IDENT ident) ->
      Printf.fprintf oc "(Quoted \"%s\")" ident
  | VECTOR list ->
      Printf.fprintf oc "(Vector [\n";
      transl_expr_sequence oc list;
      Printf.fprintf oc "])\n";
  | COMMENT c ->
(* Printf.fprintf oc "Nil (*%s*)\n" (String.escaped c)*)
      ()
  | QUOTE (LIST list) ->
      Printf.fprintf oc "(List [\n";
      transl_expr_sequence oc list;
      Printf.fprintf oc "])\n";
  | _ -> print 0 value; failwith "Bad token in transl_value" 

and transl_apply_list oc list =
  match list with
    f :: args ->
      Printf.fprintf oc "(to_function ";
      transl_expr oc f;
      Printf.fprintf oc ") [";
      transl_expr_sequence oc args;
      Printf.fprintf oc "]\n"
  | [] -> assert false
      
and transl_apply oc func list =
  match list with
    [] -> Printf.fprintf oc "fun%d" func
  | e1 :: tail ->
      incr functions;
      Printf.fprintf oc "let fun%d = (to_function fun%d) " !functions func;
      let func = !functions in
      transl_expr oc e1;
      Printf.fprintf oc "in\n";
      transl_apply oc func tail

and transl_args oc args n =
  match args with
    (IDENT "&") :: (IDENT "optional") :: tail ->
      transl_args oc tail n
  | (IDENT ident) :: tail ->
      Printf.fprintf oc "set_arg \"%s\"  %d fun_args;\n" 
        (transl_ident ident) n;
      transl_args oc tail (n+1)
  | [] -> ()
  | _ -> print_list 0 args; assert false
      
and transl_list_expr oc list =
  match list with
    (DEFVAR :: (IDENT ident) :: value :: (STRING comment) :: []) ->
      let ident = transl_ident ident in
      Printf.fprintf oc "(defvar \"%s\" " ident;
      transl_expr oc value;
      Printf.fprintf oc " \"%s\")\n" (String.escaped comment)
   | (DEFVAR :: (IDENT ident) :: value :: []) ->
      let ident = transl_ident ident in
      Printf.fprintf oc "(defvar \"%s\" " ident;
      transl_expr oc value;
      Printf.fprintf oc " \"%s\")\n" ""
  | (DEFUN :: (IDENT ident) :: (LIST args) :: tail) ->
      let ident = transl_ident ident in
      Printf.fprintf oc "(defun \"%s\" (function fun_args ->\n" ident;
      transl_args oc args 0;
      transl_expr_sequence oc tail;
      Printf.fprintf oc "))\n" 
  | (IDENT "make-variable-buffer-local") :: (
      QUOTE (IDENT ident)) :: [] ->
      Printf.fprintf oc "make_variable_buffer_local \"%s\"\n" 
        (transl_ident ident)
  | IF :: e1 :: e2 :: tail ->
      Printf.fprintf oc "(if to_boolean ";
      transl_expr oc e1;
      Printf.fprintf oc " then\n";
      transl_expr oc e2;
      Printf.fprintf oc " else (\n";
      transl_expr_sequence_or_nil oc tail;
      Printf.fprintf oc "))\n";
  | [] -> Printf.fprintf oc "Nil\n"
  | list -> transl_apply_list oc list

and transl_expr_sequence_or_nil oc list =
  match list with
    [] -> Printf.fprintf oc "Nil\n"
  | _ -> transl_expr_sequence oc list
      
and transl_expr_sequence oc list =
  match list with
    [] -> 
      ()
  | [e] -> 
      transl_expr oc e;
      Printf.fprintf oc "\n"
  | (COMMENT _) :: tail -> 
      transl_expr_sequence oc tail
  | e :: tail ->
      transl_expr oc e;
      Printf.fprintf oc ";\n";
      transl_expr_sequence oc tail
      
and transl_main oc tree = 
  match tree with
    [] -> ()
  | (LIST list) :: tail ->
      transl_list_expr oc list;
      Printf.fprintf oc ";;\n";
      transl_main oc tail
  | token :: tail ->
      begin
        match token with
          COMMENT s -> 
            Printf.fprintf oc "(*%s*)\n" s
        | _ -> 
            print 0 token;
            failwith "bad token in transl_main"
      end;
      transl_main oc tail
      
let el_to_ml file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  El_lexer.init ();
  let tree = try El_lexer.token lexbuf 
    with e ->
        Printf.printf "At pos %d" (Lexing.lexeme_end lexbuf);
        print_newline ();
        raise e
  in
  close_in ic;
  let ml_file = (ml_of_el_filename file) ^ ".ml" in
  let oc = open_out ml_file in
  Printf.fprintf oc "open El_compat;;\n";
  try
    transl_main oc tree;
    close_out oc;
  with e ->
      close_out oc;
      raise e
  
(*

(defvar name value comment) 
-->
let name = defvar "name" value comment
    *)
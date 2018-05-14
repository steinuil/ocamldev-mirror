 
{
open Lexing 
  
type token =
  IDENT of string
| COMMENT of string
| STRING of string
| LIST of token list
| LBRACE
| RBRACE
| VECTOR of token list
| FLOAT of string
| INTEGER of string
| DOT
| QUOTE of token
| COMMA
| EOF
| CHAR of char
| DEFUN
| DEFVAR
| LAMBDA
| IF
| COND
| WHILE
| SAVE_EXCURSION
| LET
| LET_STAR
| INTERACTIVE
| PROGN
| UNWIND_PROTECT
| DEFCONST
| OR 
| AND

let keywords = Hashtbl.create 101
let _ =
  List.iter (fun (kwd, token) ->
      Hashtbl.add keywords kwd token) [
    "defun", DEFUN;
    "defvar", DEFVAR;
    "lambda", LAMBDA;
    "if", IF;
    "cond", COND;
    "while", WHILE;
    "save-excursion", SAVE_EXCURSION;
    "let", LET;
    "let*", LET_STAR;
    "interactive", INTERACTIVE;
    "progn", PROGN;
    "unwind-protect", UNWIND_PROTECT;
    "defconst", DEFCONST;
    "or ", OR ;
    "and", AND;
  ]
  
let lists = ref []
let tokens = ref []
  
let init () = 
  lists := [];
  tokens := []
  
let add_token t =
  tokens := t :: !tokens
  
let push () =
  lists := !tokens :: !lists;
  tokens := []
  
let pop () =
  match !lists with
    [] -> failwith "Unstarted list"
  | old_tokens :: lists_tail ->
      lists := lists_tail;
      let list = !tokens in
      tokens := old_tokens;
      List.rev list
      
let last_pop () =
  match !lists with
    [] -> List.rev !tokens
  | _ -> failwith "Unterminated list"
      
  
let lexer_start = ref 0

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences *)

let char_for_backslash =
  match Sys.os_type with
  | "Unix" | "Win32" ->
      begin function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | "MacOS" ->
      begin function
      | 'n' -> '\013'
      | 'r' -> '\010'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | x -> failwith "Lexer: unknown system type"

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in  
  Char.chr(c land 0xFF)

(* To store the position of the beginning of a string or comment *)

let start_pos = ref 0

  
let rec after_char s c pos =
  if pos = String.length s then pos else
  if s.[pos] = c then after_char s c (pos+1)
  else pos
    
  
(* Error report *)

exception Error of int * string

}

let blank = [' ' '\009']
let return = ['\010' '\012' '\013']
let firstidentchar = 
  ['A'-'Z' 'a'-'z' '-' '_' '\192'-'\214' '\216'-'\246'
    '\248'-'\255']
let identchar = 
  ['A'-'Z' 'a'-'z' '-' '_' '\192'-'\214' '\216'-'\246' ':' '@' '=' '>' '<'
      '|'
    '\248'-'\255' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '@' '^' '|' '~']
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
      blank + | return { token lexbuf }

  | firstidentchar identchar *
    { let s = Lexing.lexeme lexbuf in
      let tok =
        try
          Hashtbl.find keywords s
        with _ -> IDENT s
      in
      add_token tok; 
      token lexbuf }
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { add_token (INTEGER (Lexing.lexeme lexbuf)); token lexbuf }
  | float_literal { add_token (FLOAT (Lexing.lexeme lexbuf)); token lexbuf }
  | "\""
      { reset_string_buffer();
      let string_start = Lexing.lexeme_start lexbuf in
      string lexbuf;
      add_token (STRING (get_stored_string ())); token lexbuf
    }
  | (* comments *)
    ";"+ [^ '\n']* '\n' { ignore (
        let s = Lexing.lexeme lexbuf in
        let len = String.length s in
        let start = after_char s ';' 0 in
        COMMENT (String.sub s start (len-1-start)));
      token lexbuf}
(* symbols *)
  | '?' {  add_token (CHAR(one_char lexbuf)); token lexbuf }
  | "'"  {  add_token (QUOTE (quoted lexbuf)); token lexbuf  }
  | "("  {  push (); add_token (LIST (token lexbuf)); token lexbuf  }
  | ")"  {  pop ()  }
  | ","  {  add_token COMMA; token lexbuf  }
  | "."  {  add_token DOT; token lexbuf  }
  | "["  {  push (); add_token (VECTOR (token lexbuf)); token lexbuf  }
  | "]"  {  pop ()  }
  | "{"  {  add_token LBRACE; token lexbuf  }
  | "}"  {  add_token RBRACE; token lexbuf  }
  | symbolchar *
    {  add_token (IDENT (Lexing.lexeme lexbuf)); token lexbuf}
  | eof { last_pop () }
  | _ {
      raise (Error (Lexing.lexeme_end lexbuf, 
          (Printf.sprintf "Unknown char [%s]" 
              (Lexing.lexeme lexbuf)))) } 

and quoted = parse
    blank + | return { quoted lexbuf }
  | "("  {  push (); LIST (token lexbuf) }
  | firstidentchar identchar * { IDENT (Lexing.lexeme lexbuf)}
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { INTEGER (Lexing.lexeme lexbuf) }
  | float_literal { FLOAT (Lexing.lexeme lexbuf) }
  | _ { failwith "Bad token in quoted" }

    
and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Error (Lexing.lexeme_end lexbuf, "Unterminated_string")) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

        
and one_char = parse
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { char_for_backslash(Lexing.lexeme_char lexbuf 1) }
  | '\\' { one_char lexbuf }
  | eof
      { raise (Error (Lexing.lexeme_end lexbuf, "Unterminated char")) }
  | _
      { Lexing.lexeme_char lexbuf 0 }


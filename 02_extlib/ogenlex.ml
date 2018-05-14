(* $Id: ogenlex.ml,v 1.2 2001/09/09 22:43:41 lefessan Exp $ *)

open Genlex

class c l = object
  inherit [token] Ostream.c [< >] as super
  val mutable input = [< >]
  val mutable start = 0
  val mutable stop = 0
  val kwd_table =
    let t = new Ohashtbl.c 17 in
    List.iter (fun x -> t#add ~key:x ~data:(Kwd x))  l;
    t
  val buffer = new Obuffer.c 32
  method start = ignore super#peek; start
  method stop = ignore super#peek; stop

  method init inp =
  let ident_or_keyword id =
    try kwd_table#find id with Not_found -> Ident id
  and keyword_or_error c =
    let s = String.make 1 c in
    try kwd_table#find s
    with Not_found -> raise(Stream.Error("Illegal character " ^ s))
  in
  let rec next_token s =
    start <- Stream.count s;
  match s with parser
    [< '  ' '|'\010'|'\013'|'\009'|'\026'|'\012'; s >] ->
      next_token s
  | [< '  'A'..'Z'|'a'..'z'|'\192'..'\255' as c; s>] ->
      buffer#reset; buffer#add_char c; ident s
  | [< '  '!'|'%'|'&'|'$'|'#'|'+'|'/'|':'|'<'|'='|'>'|'?'|'@'|'\\'|
              '~'|'^'|'|'|'*' as c; s >] ->
      buffer#reset; buffer#add_char c; ident2 s
  | [< '  '0'..'9' as c; s>] ->
      buffer#reset; buffer#add_char c; number s
  | [< '  '\''; c = char; '  '\'' >] ->
      Some(Char c)
  | [< '  '"' (* '"' *); s >] ->
      buffer#reset; Some(String(string s))
  | [< '  '-'; s >] ->
      neg_number s
  | [< '  '('; s >] ->
      maybe_comment s
  | [< ' c >] ->
      Some(keyword_or_error c)
  | [< >] ->
      None

  and ident = parser
    [< '  'A'..'Z'|'a'..'z'|'\192'..'\255'|'0'..'9'|'_'|'\'' as c; s>] ->
      buffer#add_char c; ident s
  | [< >] ->
      Some(ident_or_keyword buffer#contents)

  and ident2 = parser
    [< '  '!'|'%'|'&'|'$'|'#'|'+'|'-'|'/'|':'|'<'|'='|'>'|'?'|'@'|'\\'|
              '~'|'^'|'|'|'*' as c; s >] ->
      buffer#add_char c; ident2 s
  | [< >] ->
      Some(ident_or_keyword buffer#contents)

  and neg_number = parser
    [< '  '0'..'9' as c; s >] ->
      buffer#reset; buffer#add_char '-'; buffer#add_char c; number s
  | [< s >] ->
      buffer#reset; buffer#add_char '-'; ident2 s
    
  and number = parser
    [< '  '0'..'9' as c; s >] ->
      buffer#add_char c; number s
  | [< '  '.'; s >] ->
      buffer#add_char '.'; decimal_part s
  | [< '  'e'|'E'; s >] ->
      buffer#add_char 'E'; exponent_part s
  | [< >] ->
      Some(Int(int_of_string buffer#contents))

  and decimal_part = parser
    [< '  '0'..'9' as c; s >] ->
      buffer#add_char c; decimal_part s
  | [< '  'e'|'E'; s >] ->
      buffer#add_char 'E'; exponent_part s
  | [< >] ->
      Some(Float(float_of_string buffer#contents))

  and exponent_part = parser
    [< '  '+'|'-' as c; s >] ->
      buffer#add_char c; end_exponent_part s
  | [< s >] ->
      end_exponent_part s

  and end_exponent_part = parser
    [< '  '0'..'9' as c; s >] ->
      buffer#add_char c; end_exponent_part s
  | [< >] ->
      Some(Float(float_of_string buffer#contents))

  and string = parser
    [< '  '"' (* '"' *)  >] -> buffer#contents
  | [< '  '\\'; c = escape; s >] -> buffer#add_char c; string s
  | [< ' c; s >] -> buffer#add_char c; string s

  and char = parser
    [< '  '\\'; c = escape >] -> c
  | [< ' c >] -> c

  and escape = parser
    [< '  'n' >] -> '\n'
  | [< '  'r' >] -> '\r'
  | [< '  't' >] -> '\t'
  | [< '  '0'..'9' as c1; '  '0'..'9' as c2; '  '0'..'9' as c3 >] ->
      Char.chr((Char.code c1 - 48) * 100 +
               (Char.code c2 - 48) * 10 + (Char.code c3))
  | [< ' c >] -> c

  and maybe_comment = parser
    [< '  '*'; s >] -> comment s; next_token s
  | [< >] -> Some(keyword_or_error '(')

  and comment = parser
    [< '  '('; s >] -> maybe_nested_comment s
  | [< '  '*'; s >] -> maybe_end_comment s
  | [< ' c; s >] -> comment s

  and maybe_nested_comment = parser
    [< '  '*'; s >] -> comment s; comment s
  | [< ' c; s >] -> comment s

  and maybe_end_comment = parser
    [< '  ')' >] -> ()
  | [< ' c; s >] -> comment s

  in
    start <- 0;
    stop <- 0;
    buffer#reset;
    input <- inp;
    s <- Stream.from (fun n -> stop <- Stream.count input - 1;
      	       	       	       next_token input)
end

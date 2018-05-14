open Genlex

let ocamlconf_filename = ref ""
let ocamlver = ref ""
let values = ref []

  
let string_split s c =
  let len = String.length s in
  let rec iter pos =
    try
      if pos = len then [""] else
      let pos2 = String.index_from s pos c in
      if pos2 = pos then "" :: iter (pos+1) else
        (String.sub s pos (pos2-pos)) :: (iter (pos2+1))
    with _ -> [String.sub s pos (len-pos)]
  in
  iter 0
;;
        
let string_of_file name =
  let b = Buffer.create 10000 in
  let s = String.create 32768 in
  let ic = open_in_bin name in
  let rec iter () =
    let nread = input ic s 0 (String.length s) in
    if nread > 0 then begin
        Buffer.add_string b (String.sub s 0 nread);
        iter ()
      end
  in
  iter ();
  close_in ic;
  Buffer.contents b
;;

let file_of_string name s =  
  let oc = open_out name in
  try
    output_string oc s;
    close_out oc
  with e ->
      close_out oc;
      raise e 
;;

let lexer = Genlex.make_lexer [ "="]

let rec parse_file = parser
  [< expr = parse_expr; eof = parse_file >] -> expr :: eof
| [< >] -> []
  
and parse_expr = parser
| [< 'Ident name; 'Kwd "="; value = parse_name >] -> 
    (String.lowercase name, value)
    
and parse_name = parser
| [< 'Ident id >] -> id
| [< 'String id  >] -> id
    
let read_values filename =
  let ic = open_in filename in
  let s = Stream.of_channel ic in
  let s = lexer s in
  let list = parse_file s in
  close_in ic  ;
  list
  
let load loc filename =
    values := (read_values filename) @ !values;
    <:str_item< declare end >>
  
  
  EXTEND 
  GLOBAL: Pcaml.str_item Pcaml.expr Pcaml.patt;

Pcaml.str_item: FIRST
  [[
    "OCAMLCONF"; filename = STRING -> 
Printf.fprintf stderr "OCAMLCONF...\n"; flush stderr;
load loc filename;

| "DEFINE"; ident = UIDENT; operator = UIDENT; name = STRING ->
    let v =
      match operator with
            "EXISTS" ->
                            (try
                  let v = List.assoc name !values in
                  match String.lowercase v with
                    "yes" | "true" -> true
                  | "no" | "false" -> false
                  | _ -> failwith (Printf.sprintf "ocamlconf: Bad value [%s]" v)
                with _ -> false)
      | "VERSION_GREATER_THAN" ->
          !ocamlver >= name 
      | "VERSION_LESS_THAN" ->
          !ocamlver < name 
      | _ -> assert false
    in
    if v then
      values := (ident, "true") :: !values;
    <:str_item< declare end >>

| "IFDEF"; name = UIDENT; "THEN"; 
  e1 = Pcaml.str_item; 
  "ELSE";
  e2 = Pcaml.str_item
  ->
    if List.mem_assoc name !values then e1 else e2
    ]];

Pcaml.expr: FIRST
  [[ 
    "LOC"; e = Pcaml.expr -> 
if !ocamlver < "3.09" then
  <:expr<
  let loc = _loc in $e$  
    >> else e 
| "LLOC"; e = Pcaml.expr -> 
if !ocamlver < "3.09" then
  <:expr<
  let _loc = loc in $e$  
          >> else e 
| "IFDEF"; name = UIDENT; "THEN"; 
  e1 = Pcaml.expr; 
  "ELSE";
  e2 = Pcaml.expr
 ->
    if List.mem_assoc name !values then e1 else e2
          
          ]];

Pcaml.patt: FIRST
  [[
 "IFDEF"; name = UIDENT; "THEN"; 
  e1 = Pcaml.patt; 
  "ELSE";
  e2 = Pcaml.patt
  ->
    if List.mem_assoc name !values then e1 else e2
    
    ]];

  END;;
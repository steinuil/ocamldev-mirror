let string_ncmp s1 s2 n =
  let sz1 = String.length s1 in
  let sz2 = String.length s2 in
  if sz1 < n || sz2 < n then s1 = s2
  else
    let s1' = String.sub s1 0 n in
    let s2' = String.sub s2 0 n in
    s1' = s2'

(* search str in doc from char deb *)
let search_from doc deb str =
  let last = (String.length doc) - (String.length str) in
  let i = ref deb in
  let cont = ref true in
  let len = String.length str in
  let ok () =
    try
      for j=0 to len-1 do
        if doc.[!i+j] != str.[j] then raise Not_found
      done;
      true
    with _ -> false in
  while !cont do
    (* make sure we're not too far away *)
    if !i > last
    then raise Not_found;
    (* Is it ok ? *)
    if ok() then cont := false else incr i
  done;
  !i

(* replace all the occurences of a char in a string by a given string *)
let replace doc chr str =
  let res = Buffer.create (2 * (String.length doc)) in
  let pos = ref 0 in
  let rec aux () =
    let new_pos = String.index_from doc !pos chr in
    Buffer.add_substring res doc !pos (new_pos - !pos);
    Buffer.add_string res str;
    pos := new_pos + 1;
    aux () in
  (try
    aux ()
  with
  | Not_found -> Buffer.add_substring res doc !pos ((String.length doc) - !pos)
  | Invalid_argument _ -> ());
  Buffer.contents res

let split s c =
  let len = String.length s in
  let rec iter pos =
    try
      if pos = len then [""] else
      let pos2 = String.index_from s pos c in
      (String.sub s pos (pos2-pos)) :: (iter (pos2+1))
    with _ -> [String.sub s pos (len-pos)]
  in
  iter 0
;;

let rec unsplit l c =
  match l with
    [] -> ""
  | [x] -> x
  | x :: ((y :: l) as tail) ->
      Printf.sprintf "%s%c%s" x c (unsplit tail c)
;;

let words s = 
  let len = String.length s in
  let rec iter_out pos =
    if pos = len then [] else
    let c = s.[pos] in
    match c with
      ' ' | '\009' | '\010' | '\013' -> iter_out (pos+1)
      | _ -> iter_in pos (pos+1)
        
  and iter_in pos0 pos =
    if pos = len then [String.sub s pos0 (len-pos0)] else
    let c = s.[pos] in
    match c with
      ' ' | '\009' | '\010' | '\013' -> 
        (String.sub s pos0 (pos - pos0))::
        (iter_out (pos+1))
      | _ -> iter_in pos0 (pos+1)
  in
  iter_out 0
;;

let convert init f s =
  let len = String.length s in
  let b = Buffer.create len in
  let status = ref init in
  for i = 0 to len - 1 do
    let c = s.[i] in
    status := f b !status c
  done;
  Buffer.contents b

let before s pos = String.sub s 0 pos
let after s pos = 
  let len = String.length s in
  String.sub s pos (len - pos)
  
let cut_at s c =
  try
    let pos = String.index s c in
    before s pos,
    after s (pos+1);
  with _ -> s, ""

      
let check_prefix s prefix =
  let len = String.length s in
  let plen = String.length prefix in
  len >= plen  && String.sub s 0 plen = prefix
  
let upp_initial s =
  if String.length s > 0 then
    let s = String.copy s in
    s.[0] <- Char.uppercase s.[0]; s
  else
    s
    
(* not optimal !*)
let rec subequal s1 pos1 s2 pos2 len =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  pos1 + len <= len1 &&
  pos2 + len <= len2 &&
  (let rec iter pos =
      pos = len ||
      (s1.[pos1 + pos] = s2.[pos2 + pos] && iter (pos+1))
    in
    iter 0)
    
(* not optimal !*)
let subcontains s sub =
  let slen = String.length sub in
  let len = String.length s in
  len >= slen && (
    let rec after_pos pos =
      not (pos + slen > len) &&
      (subequal s pos sub 0 slen ||
      after_pos (pos+1))        
    in 
    after_pos 0)
  
let of_char c = String.make 1 c
  
  
let resize s newlen =
  let len = String.length s in
  if len > newlen then String.sub s 0 newlen 
  else
  let str = String.create newlen in
  String.blit s 0 str 0 len;
  str
  

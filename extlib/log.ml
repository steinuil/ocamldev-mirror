
(*
let lock = ref false
let discard = ref true
let list = ref []

let enable () = discard := false
let disable () = discard := true
let unlock () =
  lock := false;
  let rec iter l =
    match l with
      [] -> list := []
    | s :: tail -> print_string s; iter tail
  in
  iter (List.rev !list);
  flush stdout

let p s = 
  if !discard then () else
  if !lock then list := s :: !list 
  else print_string s; flush stdout

let lock () = lock := true
  *)

let logp = ref false
let log_name = ref ((Filename.basename Sys.argv.(0))^".log")
  
let oc = ref None  
let outc () =
  match !oc with
    None -> 
      let outc = open_out !log_name in 
      oc := Some outc; outc
  | Some outc -> outc
      
let printf f x = 
  if !logp then
    let oc = outc () in
    Printf.fprintf oc f x;
    flush oc
    
let exn f e =
  if !logp then
  let oc = outc () in
  Printf.fprintf oc f (Printexc.to_string e);
  flush oc

let catch format f =
  try f () with e -> exn format e
      
let watch format f =
  try f () with e -> exn format e; raise e
    
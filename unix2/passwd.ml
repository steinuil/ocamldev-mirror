(* ??? We should probably remove this file and use the fgetpwent, etc 
  functions already provided by Unix. Not sure... *)


open Cdk_passwd_lexers
open Cdk_passwd_parsers

let check_prefix filename prefix =
  let len = String.length prefix in
  let n = String.length filename - len in
  if n < 0 then raise Not_found;
  for i = 0 to len-1 do
    if filename.[i] <> prefix.[i] then raise Not_found
  done;
  String.sub filename len n
  
let replace_prefix prefix replace filename entries =
  List.fold_left (fun filename entry ->
      try
        let fin = check_prefix filename (prefix entry) in
        (replace entry) ^ fin
      with
        Not_found -> filename) filename entries
  
let home = try Sys.getenv "HOME" with Not_found -> "/"

type entry = {
    login : string;
    uid : int;
    gid : int;
    name : string;
    homedir : string;
    shell : string;
  }

let entries = ref None
  
let get_entries () =
  match !entries with
    Some v -> v
  | None ->
      let ic = open_in "/etc/passwd" in
      let file = 
        try
          parse_passwd lexer_passwd (Lexing.from_channel ic) 
        with
          _ -> close_in ic; []
      in
      let rec iter file entries =
        match file with
          [] -> entries
        | entry :: file ->
            let entry = 
              try
                match entry with
                  login :: uid :: gid :: name :: homedir :: shell :: _ -> 
                    Some
                      { login = login;
                      uid = int_of_string uid;
                      gid = int_of_string gid;
                      name = name;
                      homedir = homedir;
                      shell = shell
                    }
                | _ -> None
              with _ -> None
            in
            match entry with 
              None -> iter file entries
            | Some entry -> iter file (entry :: entries)
      in 
      let v = iter file [] in
      entries := Some v;
      v

let clear_entries () = entries := None
      
let fake_entry = 
  { login = ""; homedir = home;
    uid = 0; gid = 0; shell = "/bin/sh"; name = ""; }

let homedir entry = entry.homedir
let login entry = "~" ^ entry.login
  
let filename_to_string filename =
  replace_prefix homedir login filename (fake_entry :: (get_entries ()))
  
let string_to_filename filename =
  replace_prefix login homedir filename (fake_entry :: (get_entries ()))
  
let _ =
  Filename2.register_conversions string_to_filename filename_to_string
  
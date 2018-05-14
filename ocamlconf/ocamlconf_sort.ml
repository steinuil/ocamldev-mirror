(* 
BUGS:
 * all seen files should be loaded everytime
 * .mli files are not copied in tarballs
 * assertion at line 737 should be replaced by message 
*)

open Genlex
  
(* Read the configuration of the system *)

let exit2 s =
  Printf.printf "Error: %s\n" s;
  exit 2

let warning s =
  Printf.printf "Warning: %s\n" s
  
let lexer = Genlex.make_lexer [ "="; "begin"; "end" ]

let is_directory filename = 
  try
    let st = Unix.stat filename in
    st.Unix.st_kind = Unix.S_DIR
  with _ -> false

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

let list_directory filename =
  let dir = try Unix.opendir filename 
    with    
      | e ->
        exit2 (Printf.sprintf "Warning: cannot list %s" filename);
  in
  let list = ref [] in
  try
    while true do
      let file = Unix.readdir dir in 
      if file <> "." && file <> ".." then begin
          list := file :: !list 
        end;
    done;
    assert false
  with 
    End_of_file -> 
      Unix.closedir dir;
      !list
      
      

let is_directory filename =
  try let s = Unix.stat filename in s.Unix.st_kind = Unix.S_DIR with _ -> false

let is_link filename =
  try let s = Unix.lstat filename in s.Unix.st_kind = Unix.S_LNK with _ -> false

let rec safe_mkdir dir =  
  if Sys.file_exists dir then begin
      if not (is_directory dir) then 
        failwith (Printf.sprintf "%s not a directory" dir)
    end
  else 
  if is_link dir then
    failwith (Printf.sprintf "%s is an orphan symbolic link" dir)
  else begin
      let predir = Filename.dirname dir in
      if predir <> dir then safe_mkdir predir;
      if not (Sys.file_exists dir) then 
        Unix.mkdir dir 0o775 
    end    

let rec remove_empty list list2 =
  match list with
    [] -> List.rev list2
  | "" :: list -> remove_empty list list2
  | s :: list -> remove_empty list (s :: list2)

let simplify_blanks s =
  for pos = 0 to String.length s - 1 do
    match s.[pos] with
      '\t' | '\n' -> s.[pos] <- ' '
    | _ -> ()
  done;
  s
      
let split_simplify s c =
  let list = string_split (simplify_blanks s) c in
  remove_empty list []

module Path = struct  
let rec normalize path =
  match path with
    [] -> []
  | dir :: tail ->
      let dir = dir :: normalize tail in
      match dir with
      | "/" :: path -> path
      | "" :: path -> path
      | "." :: path -> path
      | ".." :: path -> dir
      | _ :: ".." :: path -> path
      | dir -> dir
    
let simplify (root, path) =
  let path = normalize path in
  (root, path)
    
let to_path filename =
  let filename = String.copy filename in
  let len = String.length filename in
  for i = 0 to len - 1 do
    if filename.[i] = '\\' then filename.[i] <- '/';
  done;
  if len > 2 && filename.[1]  = ':' &&
    match filename.[0] with 
      'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false then
    Some (String.sub filename 0 2),
    split_simplify (String.sub filename 2 (len-2)) '/'
  else 
  let list = split_simplify filename '/' in
  if len > 1 && filename.[0] = '/' then
    Some "/", list
  else None, list

let of_path (root, path) =
  let rec iter dir list =
    match list with
      [] -> dir
    | filename :: tail ->
        iter (Filename.concat dir filename) tail
  in
  match path with
    [] -> begin
        match root with
          None -> "" 
        | Some root -> root
      end
  | dir :: tail -> 
      let path = iter dir tail in
      match root with
        None -> path
      | Some root ->
          Filename.concat root path

end

let simplify_filename filename =
  let path = Path.to_path filename in
  Path.of_path (Path.simplify path)

  
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
  
let file_of_string name s =
  
  let oc = open_out name in
  try
    output_string oc s;
    close_out oc
  with e ->
      close_out oc;
      raise e 

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
      let rec try_dir = function
          [] -> raise Not_found
        | dir::rem ->
            let fullname = Filename.concat dir name in
            if Sys.file_exists fullname then fullname else try_dir rem
      in try_dir path
    end

let extension file =
  try
    let len = String.length file in
    let pos = String.rindex file '.' in
    String.sub file pos (len-pos)
  with _ -> ""

let current_dir = Unix.getcwd ()
let home = Sys.getenv "HOME"
let path = Sys.getenv "PATH"
  
let path = string_split path ':'

let bin_name = Sys.argv.(0)
let bin_name = 
  if Filename.basename bin_name = bin_name then
    try
      find_in_path path bin_name
    with Not_found -> bin_name
  else 
  if Filename.is_relative bin_name then
    Filename.concat current_dir bin_name
  else bin_name
  

(* let scan_directories = ref []*)

let arg_list = [
(*    "-scan", Arg.String (fun s ->
        scan_directories := s :: !scan_directories), " : scan directories looking for ocamlconf files"; *)
    ]
let usage = Printf.sprintf "Ocaml Configurator:\nUsage: %s\n%!" Sys.argv.(0)
let anon_fun _ = Printf.eprintf "%s: No anonymous function\n" Sys.argv.(0); Arg.usage arg_list usage; exit 2

let _ =
  Arg.parse arg_list anon_fun usage

type expr =
| SetVariable of string * string
| Project of string * expr list
  
let rec parse_file = parser
| [< expr = parse_expr; eof = parse_file >] -> expr :: eof
| [< >] -> []
  
and parse_expr = parser
| [< 'Kwd "begin"; name = parse_name; list = parse_file; 'Kwd "end" >] ->
    Project (name, list)
| [< 'Ident name; 'Kwd "="; valeur = parse_name >] -> 
    SetVariable (String.lowercase name, valeur)
    
and parse_name = parser
| [< 'Ident id >] -> id
| [< 'String id  >] -> id
    
let read_valeurs filename =
  let ic = open_in filename in
  let s = Stream.of_channel ic in
  let s = lexer s in
  let list = parse_file s in
  close_in ic  ;
  list
  
  (*
  let rec iter0 () =
    let token = Stream.next l in
    match token with 
      Genlex.Ident s -> iter1 s
    | _ -> failwith "Expecting variable name"
  
  and iter1 s =
    let token = try Stream.next l 
      with Stream.Failure -> failwith "unexpected end of file"  
    in
    match token with 
      Genlex.Kwd "=" -> iter2 s
    | _ -> failwith "Expecting = sign"
  and iter2 s1 =
    let token = 
      try
        Stream.next l
      
      with Stream.Failure -> failwith "unexpected end of file"  
    in
    let s2 =
      match token with 
      | Genlex.String s -> s
      | Genlex.Int i -> string_of_int i
      | Genlex.Ident s -> s
      | Genlex.Float f -> string_of_float f
      | _ -> failwith "Expecting valeur"
    in
    f (String.lowercase s1) (simplify_blanks s2);
    iter0 ()
  in
  (try
      iter0 ()
    with Stream.Failure -> ()); *)
(*
  try
    while true do
      let line = input_line ic in
      let len = String.length line in
      if len > 0 && line.[0] <> '#' then
        let pos = String.index line '=' in
        let name = String.lowercase (String.sub line 0 pos) in
        let valeur = String.sub line (pos+1) (len - pos - 1) in
        f name valeur
    done
      
  with End_of_file -> 
      close_in ic
*)

open Ocamlconf_config
  
let ocamlc = 
  try
    find_in_path path "ocamlc.opt"
  with Not_found ->
      find_in_path path "ocamlc"

let only_bytecode = ref false
      
let ocamlopt = 
  try
    find_in_path path "ocamlopt.opt"
  with Not_found ->
      try
        find_in_path path "ocamlopt"
      with Not_found ->
          only_bytecode := true;
          ""

let ocamldep = 
  try
    find_in_path path "ocamldep.opt"
  with Not_found ->
      find_in_path path "ocamldep"

let ocamllex = 
  try
    find_in_path path "ocamllex.opt"
  with Not_found ->
      find_in_path path "ocamllex"

let ocamldoc = 
  try
    find_in_path path "ocamldoc.opt"
  with Not_found ->
      find_in_path path "ocamldoc"
  
      
  
let _ =
  
  let ic = Unix.open_process_in (Printf.sprintf "%s -version" ocamlc) in
  let ocamlc_version = input_line ic in
  ignore (Unix.close_process_in ic);
  if ocamlc_version <> ocamlver then begin
      Printf.fprintf stderr "Error: ocamlconf has been compiled with ocamlc version %s\n" ocamlver;
      Printf.fprintf stderr "  current version is %s\n. You must use the same version." ocamlc_version;
      exit 2      
    end  
    
type kind = LIBRARY | PROGRAM | PREPROCESSOR | PACKAGE | CONFIGURE
  
type project = {
    mutable pj_num : int;
    mutable pj_files : string list;
    mutable pj_dirname : string;
    mutable pj_libname : string;
    mutable pj_requires : string list;
    mutable pj_enabled : bool;
    mutable pj_type : kind;
    mutable pj_pp : string;
    mutable pj_score : int;
    mutable pj_deps : bool array;
    mutable pj_rules : string;
    mutable pj_cclib : string;
    mutable pj_cflags : string;
    mutable pj_installed : bool;
    mutable pj_interfaces : string;
    mutable pj_internal : bool;
    mutable pj_packages : string list;
    mutable pj_included : string list;
    mutable pj_version : string;
    mutable pj_generated : string list;
    mutable pj_script : string;
  }
  
let packages = ref []
let programs = ref []
let projects = ref []
let preprocessors = ref []
  
let projects_by_name = Hashtbl.create 13
let installed = ref false
  
let new_project () = {
    pj_num = -1;
    pj_requires = [];
    pj_files = [];
    pj_dirname = "";
    pj_libname = "";
    pj_enabled = true;
    pj_type = LIBRARY;
    pj_pp = "";
    pj_score = 0;
    pj_deps = [||];
    pj_rules = "";
    pj_cclib = "";
    pj_cflags = "";
    pj_interfaces = "";
    pj_installed = !installed;
    pj_internal = false;
    pj_packages = [];
    pj_included = [];
    pj_version = "0.01";
    pj_generated = [];
    pj_script = "";
  } 
  
let copy_project pj =
  let pj = { pj with pj_rules = pj.pj_rules } in
  pj  

let declare_project pj = 
(*  if pj.pj_enabled then *)
    let pj  = copy_project pj in
(* Remove old version first *)
    if Hashtbl.mem projects_by_name pj.pj_libname then
      Hashtbl.remove projects_by_name pj.pj_libname;
    Printf.printf "(%s)%!" pj.pj_libname;
    Hashtbl.add projects_by_name pj.pj_libname pj

exception BadSystem
    
let rec parse_exprs pj elist =
  
  List.iter (parse_expr pj) elist

  
  
and parse_expr pj e =
  match e with
  | Project (name, elist) ->
      
      let pj = copy_project pj in
      pj.pj_libname <- name;
      parse_exprs pj elist;
      declare_project pj
  
  | SetVariable (name, valeur) ->
      match name with
      | "include" -> load_file pj valeur
      | "internal" -> pj.pj_internal <- bool_of_string valeur
      | "files" -> 
          if String.lowercase valeur = "auto" then
            let list = list_directory pj.pj_dirname in
            List.iter (fun oldfile ->
                try
                  let len = String.length oldfile in
                  let pos = String.rindex oldfile '.' in
                  let ext = String.sub oldfile (pos+1) (len-pos-1) in
                  let file = String.sub oldfile 0 pos in
                  match ext with
                    "ml" ->
                      if not (Sys.file_exists (file ^ ".mll")) &&
                        not (Sys.file_exists (file ^ ".mly")) then
                        pj.pj_files <- oldfile :: pj.pj_files
                  | "mly" ->
                      pj.pj_files <- oldfile :: pj.pj_files
                  | "mll" 
                  | ".c"
                  | ".mli"
                    ->
                      pj.pj_files <- oldfile :: pj.pj_files
                  | _ -> ()
                with _ ->()
            ) list
          else
            pj.pj_files <- pj.pj_files @ split_simplify valeur ' '
      | "interfaces" -> pj.pj_interfaces <- pj.pj_interfaces ^ " " ^ valeur
      | "name" -> pj.pj_libname <- valeur
      | "requires" -> 
          let reqs = split_simplify valeur ' ' in
          List.iter (fun name -> Printf.printf "(%s>)" name) reqs;
          pj.pj_requires <- pj.pj_requires @ reqs
      | "generated" -> pj.pj_generated <- pj.pj_generated @ split_simplify valeur ' '
      | "packages" -> pj.pj_packages <- pj.pj_packages @ split_simplify valeur ' '
      | "system" -> 
          if valeur <> "unix" then  raise BadSystem
(* pj.pj_enabled <- pj.pj_enabled && (valeur = "unix") *)
      
      | "version_min" ->
          pj.pj_enabled <- pj.pj_enabled && (valeur <= ocamlver)
      | "version_max" ->
          pj.pj_enabled <- pj.pj_enabled && (valeur >= ocamlver)
      | "installed" -> 
          (match String.lowercase valeur with
              "yes" | "true" -> pj.pj_installed <- true
            | "no" | "false" -> pj.pj_installed <- false
            | _ -> ())
      
      | "enabled" ->
          (match String.lowercase valeur with
              "yes" | "true" -> pj.pj_enabled <- true
            | "no" | "false" -> pj.pj_enabled <- false
            | _ -> ())
      | "type" ->
          pj.pj_type <- (match valeur with
            | "library" -> LIBRARY
            | "program" -> PROGRAM
            | "preprocessor" | "pp" -> PREPROCESSOR
            | "package" -> PACKAGE
            | "configure" -> CONFIGURE
            | name -> exit2 (Printf.sprintf "Unknown project type '%s' \n" name)
          )
      | "pp" ->  pj.pj_pp <- valeur
      | "script" ->  pj.pj_script <- valeur
      | "cclib" ->
          pj.pj_cclib <- pj.pj_cclib ^ " " ^ valeur
      | "cflags" -> 
          pj.pj_cflags <- pj.pj_cflags ^ " " ^ valeur
      | _ ->
          warning (Printf.sprintf "Unknown variable '%s'" name)
          
and load_file pj filename =
  pj.pj_rules <- filename;
  let elist = read_valeurs filename in
  parse_exprs pj elist    

let load_rules pj filename = 
  try
    let pj = copy_project pj in
    Printf.printf "Load rules from %s...%!" filename;
    load_file pj filename;
    if pj.pj_libname <> "" then begin
        declare_project pj;
        pj.pj_libname <- "";
      end;
    Printf.printf "done\n%!"
  with BadSystem -> ()
      
type state = {
    state_configures :  string list ref;
    state_devel_projects :  string list ref;
    state_installed_projects :  string list ref;
  }

type dependency =
  DepNormal
| DepOptional
| DepRequired
  
let required_name name =
  if name.[0] = '?' then 
    (String.sub name 1 (String.length name - 1), DepOptional) 
  else
  if name.[0] = '!' then 
    (String.sub name 1 (String.length name - 1), DepRequired) 
  else    
    (name, DepNormal)

(*
let state_filename = "ocamlconf.state"
  
let state =
  if Sys.file_exists state_filename then begin
      let ic = open_in state_filename in
      let state = input_valeur ic in
      close_in ic;
      state 
    end else begin
      scan_directories := "." :: !scan_directories;
      *)
let state =
  { state_devel_projects = ref []; state_installed_projects = ref []; state_configures = ref [] }
    

let configures = ref []

let add_ocamlconf filename list =
  if not (List.mem filename !list) then begin
      Printf.printf "New file %s\n%!" filename;
      list := filename :: !list
    end
  
let _ =  
  let rec iter_scan dirname =
    let subdirs = ref [] in
    
    let list = list_directory dirname in
    List.iter (fun s ->
        let filename = Filename.concat dirname s in
        if is_directory filename then
          subdirs := filename :: !subdirs
        else
        if extension s = ".ocf" then begin
            if !installed then
              add_ocamlconf filename state.state_installed_projects
            else
              add_ocamlconf filename state.state_devel_projects
          end;
        if extension s = ".occ" then
          add_ocamlconf filename state.state_configures
    ) list;
    List.iter iter_scan !subdirs
  in
  installed := false;
  
  List.iter iter_scan ["."];
  
  let rec iter filename =
    let dirname = Filename.dirname filename in
    let pj = 
      if dirname = filename then
        new_project () 
      else
        iter dirname
    in
    pj.pj_dirname <- dirname;
    if is_directory filename then
      let included = Filename.concat filename "ocamlconf.oci" in
      if Sys.file_exists included then begin
          load_file pj included;
          pj.pj_included <- included :: pj.pj_included;
        end;
      pj
    else 
    if Sys.file_exists filename then
      (load_rules pj filename; pj)
    else begin
        warning (Printf.sprintf "'%s' has disappeared" filename);
        pj
      end
  in

(*
  if !scan_directories then begin
      
      List.iter (fun init ->
          Printf.printf "config: %s...%!" init;
          let generated = ref [] in
          let generator = ref [] in
          let script = ref "./configure" in
          let list = read_valeurs init in
          List.iter (fun e ->
              match e with
              | SetVariable (name, valeur) ->
                  begin
                    match name with
                      "generated" -> generated := split_simplify valeur ' '
                    | "generator" -> generator := split_simplify valeur ' '
                    | "script" -> script := valeur                        
                    | _ -> ()
                  end
              | _ -> ()
          ) list;
          Printf.printf "done\n";
          configures := (init, !script, !generator, !generated) :: !configures
          
      ) state.state_configures      
      
    end;
*)
  
  installed := true;
  List.iter (fun filename ->
      if filename <> "" then ignore (iter filename)
  ) (List.sort compare !(state.state_installed_projects));

  installed := false;
  List.iter (fun filename ->
      if filename <> "" then ignore (iter filename)
  ) (List.sort compare !(state.state_devel_projects));
  Printf.printf "Done with loading\n%!"

let all_projects =
(* build the lists *)
  Printf.printf "Projects: ";
  let all_projects = ref [] in
  Hashtbl.iter (fun _ pj ->
      all_projects := pj :: !all_projects;
  ) projects_by_name;
  List.iter (fun pj ->
      Printf.printf "%s " pj.pj_libname;
      let projects = match pj.pj_type with
        | LIBRARY -> projects
        | PREPROCESSOR -> preprocessors
        | PROGRAM -> programs
        | PACKAGE -> packages
        | CONFIGURE -> configures
      in
      projects := pj :: !projects;
      List.iter (fun name ->
          let pp = 
            try Hashtbl.find projects_by_name name
            with Not_found ->
                let pp = new_project () in
                pp.pj_libname <- name;
                pp.pj_type <- PACKAGE;
                Printf.printf "PACKAGE %s\n" name;
                packages := pp :: !packages;
                all_projects := pp :: !all_projects;
                Hashtbl.add projects_by_name name pp;
                pp
          in
          assert (pp.pj_type = PACKAGE);
      ) pj.pj_packages
      
  ) !all_projects;
  Printf.printf "\n";
  !all_projects

(* Topological sort on projects *)
  
let _ =
  let changed = ref true in
  Printf.printf "Topological sort...%!";
  while !changed do
    changed := false;
    List.iter (fun pj ->
        let score = pj.pj_score + 1 in
        List.iter (fun name ->
            try
              let name, _ = required_name name in
              let pj = Hashtbl.find projects_by_name name in
              if pj.pj_score < score then begin
                  changed := true;
                  pj.pj_score <- score
                end
            with Not_found -> ()
        ) pj.pj_requires;
        try
          let pj = Hashtbl.find projects_by_name pj.pj_pp in
          if pj.pj_score < score then begin
              changed := true;
              pj.pj_score <- score
            end
        with Not_found -> ()
    ) all_projects
  done;
  Printf.printf "done\n%!"

let projects = Array.of_list (
    List.sort (fun p1 p2 -> - (compare p1.pj_score p2.pj_score)) !projects)

let all_projects = Array.of_list (
    List.sort (fun p1 p2 -> - (compare p1.pj_score p2.pj_score)) all_projects)

let nprojects = Array.length projects

let compute_dependencies pj =
  pj.pj_deps <- Array.create nprojects false;
  let reqs = pj.pj_requires in
  pj.pj_requires <- [];
  List.iter (fun name ->
      let name, required = required_name name in
      try
        let p2 = Hashtbl.find projects_by_name name in
        if not p2.pj_enabled then raise Not_found; 
        pj.pj_deps.(p2.pj_num) <- true;
(*        Printf.printf "\t\t -> %s (%d)\n" p2.pj_libname p2.pj_num; *)
        List.iter (fun r ->
            if not (List.mem r pj.pj_requires) then
              pj.pj_requires <- r :: pj.pj_requires
        ) p2.pj_requires;
        for i = 0 to nprojects - 1 do
          pj.pj_deps.(i) <- pj.pj_deps.(i) || p2.pj_deps.(i);
(*          if pj.pj_deps.(i) then begin
              Printf.printf "\t\t\tPROJECT %d: %s\n" i projects.(i).pj_libname;
            end *)
        done
      with _ ->
          Printf.printf "\t\t[%s] not found\n" name; 
          match required with
            DepNormal ->
              pj.pj_requires <- name :: pj.pj_requires
          | DepOptional -> ()
          | DepRequired ->
              Printf.printf "\t\t\t->required: disable %s\n" pj.pj_libname;
              pj.pj_enabled <- false
  ) reqs;
  pj.pj_requires <- List.rev pj.pj_requires

let _ =
  Array.iteri (fun i pj ->
      pj.pj_num <- i;
      compute_dependencies pj;
  ) projects;
  List.iter compute_dependencies !programs;
  List.iter compute_dependencies !preprocessors;
  List.iter compute_dependencies !packages;
  List.iter compute_dependencies !configures

  
let _ =
  Array.iter (fun pj ->
      if pj.pj_enabled then
        List.iter (fun name ->
            let pp = Hashtbl.find projects_by_name name in
            if not (List.mem pj.pj_libname pp.pj_files) then 
              pp.pj_files <- pj.pj_libname :: pp.pj_files
      ) pj.pj_packages
      
  ) all_projects

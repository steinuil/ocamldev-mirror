(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Gwml_version  
(*************************************
  
          Arguments
  
*************************************)

let gwml_version = gwml_version ^ "-" ^ gwml_release
let gwml_lib = gwml_lib
  
let mapall = ref false

let load_path = define_option ["gwml_path"] 
  "<load_path> is the path where modules (.cmo and .cma) can be found
  for dynamic linking." path_option []

let path = ref [] (* Dyneval.load_path *)
  
let gwml_path = [
    (Filename.concat Passwd.home (".gwml-" ^ gwml_version));
    gwml_lib;
    Cdk.ocamllib;
    Filename.concat Cdk.ocamllib "xlib";
    Filename.concat Cdk.ocamllib "ocamlsrc"; 
    ]

let _ = 
  path := !!load_path @ gwml_path;
  option_hook load_path (fun _ ->
      path := !!load_path @ gwml_path)

let dpyname = ref ""
let args = ref []
let retry = ref false
let no_gwmlrc = ref false
let load_files = ref []
let batch_mode = ref false
let gwml_talk = ref false
let wm_mods = ref ""
let help = ref false
let install = ref false  
let loop = ref false
let _ = 
  
  Printf.printf "This is Gwml, the Generic Window Manager in ML\nVersion %s"
    gwml_version;
  print_newline ();
  
  Arg.parse 
    [
    "-d", Arg.String (fun s -> dpyname :=s), " <display>: set display name";
    "--display", Arg.String (fun s -> dpyname :=s), " <display>: set display name";
    "-I", Arg.String (fun s -> load_path =:= s :: !!load_path)," <dir>: add dir to load path";
    "-M", Arg.Set mapall, ": map all withdrawn windows";
    "-r", Arg.Set retry, ": retry infinitely";
    "-q", Arg.Set no_gwmlrc, ": do not load gwmlrc.cmo";
    "-f", Arg.String (fun f -> load_files := f :: !load_files),
    " <file.cmo>: load file.cmo";
(*    "-c", Arg.String Dyneval.compile,"<file.ml>: compile file"; *)
    "-batch", Arg.Set batch_mode, ": run in batch mode";
    "-talk", Arg.Set gwml_talk,": talk with Gwml on stdin/stdout";
    "-mods", Arg.String (fun s -> wm_mods := s), ": set short-keys modifier";
    "-options", Arg.Set help,": print help on options";
    "-install", Arg.Set install, ": smart install";
    "-log", Arg.Set Log.logp, ": set logging of operations";
    "-loop", Arg.Set loop, ": enter event loop, event in batch mode";
  ] (fun s -> args := s :: !args) "Gwml: an extensible window manager"

let _ = 
  args := List.rev !args;
  load_files := List.rev !load_files
  
  
let _ =
  Options.filename := (try Filepath.find_in_path (Passwd.home :: !!load_path)
      ".gwmlrc"
    with _ -> Filename.concat Passwd.home ".gwmlrc");
  (try Options.load () with _ -> ())

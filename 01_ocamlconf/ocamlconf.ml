(* 
BUGS:
 * all seen files should be loaded everytime
 * .mli files are not copied in tarballs
* assertion at line 737 should be replaced by message 
* dependencies towards configuration scripts


TODO:
1) proposition:
Instead of rebuilding all the dependencies when one ocamlconf.ocf has been
modified, generate a file Makefile_after, which contains only the dependencies
to be rebuilt. The rule that calls ocamlconf is then followed with
$(MAKE) -f Makefile.after depend

The only packages whose dependencies should be rebuilt are those depending
  on the library.
2) add dependencies toto.depend: .../ocamlconf.ocf   

Split into :

ocamlconf -scan .
./configure
ocamlmake -scan .
make all
make install
*)

open Genlex
open Unix
open Ocamlconf_sort
open Ocamlconf_config

let conf_oc = open_out "configure"
let make_oc = open_out "Makefile.configure"

let _ =
  if not (Sys.file_exists "Makefile.config") then
    let make4_oc = open_out "Makefile.config" in
    Printf.fprintf make4_oc "CC = gcc -g\n";
    Printf.fprintf make4_oc "AR= ar rc\n";
    Printf.fprintf make4_oc "RANLIB = ranlib\n";
    Printf.fprintf make4_oc "OCAMLC=%s -g $(OFLAGS)\n" ocamlc;
    Printf.fprintf make4_oc "OCAMLOPT=%s $(OFLAGS)\n" ocamlopt;
    Printf.fprintf make4_oc "OCAMLDEP=%s\n" ocamldep;
    Printf.fprintf make4_oc "OCAMLLEX=%s\n" ocamllex;
    Printf.fprintf make4_oc "OCAMLYACC=%s\n" (Filename.concat ocamlbin "ocamlyacc");
    Printf.fprintf make4_oc "OCAMLDOC=%s\n" ocamldoc;
    Printf.fprintf make4_oc "OCAMLVERSION=%s\n" ocamlver;
    Printf.fprintf make4_oc "OCAMLDIR=%s\n" ocamllib;
    Printf.fprintf make4_oc "CP=cp -f\n";
    Printf.fprintf make4_oc "\n\n";
    Printf.fprintf make4_oc "HTML_DIR=doc/html\n";
    Printf.fprintf make4_oc "MAN_DIR=doc/man\n";
    close_out make4_oc;
    ()
    
  
let build_project pj =
  let name = pj.pj_libname in
  match pj.pj_type with
  
  | CONFIGURE ->
      
      Printf.fprintf make_oc "%s_GENERATED=" name;
      List.iter (fun name -> Printf.fprintf make_oc "%s " (simplify_filename (Filename.concat pj.pj_dirname name))) pj.pj_generated;
      Printf.fprintf make_oc "\n"; 
      
      Printf.fprintf make_oc "%s_GENERATOR=" name;
      List.iter (fun name -> Printf.fprintf make_oc " %s " (simplify_filename (Filename.concat pj.pj_dirname name))) pj.pj_files;
      Printf.fprintf make_oc "\n";
      
      Printf.fprintf make_oc "$(%s_GENERATED): $(%s_GENERATOR)" name name;
      Printf.fprintf make_oc "\n\tcd %s; %s\n" pj.pj_dirname pj.pj_script;
      
      Printf.fprintf make_oc "configuration:: $(%s_GENERATED)\n" name  
  | _ -> ()
  


let _ =   Array.iter build_project all_projects
  
    
let _ =
  Printf.fprintf conf_oc "#!/bin/sh\n\n";
  Printf.fprintf conf_oc "make -f Makefile.configure configuration\n\n";
  Printf.fprintf conf_oc "%s\n\n" (
    if Sys.argv.(0) = "ocamlconf" then "ocamlmake" else
    Filename.concat (Filename.dirname Sys.argv.(0)) "ocamlmake");
  close_out conf_oc;
  close_out make_oc;
  Unix.chmod "configure" 0o755

let _ =
  Printf.printf "\n";
  Printf.printf "1) ./configure\n";
  Printf.printf "2) ocamlmake\n";
  Printf.printf "3) make\n";
  
(*  
let _ =

  List.iter build_project !preprocessors;
  Array.iter build_project projects;
  List.iter build_project !programs;  
  List.iter build_project !configures;
  List.iter build_project !packages;
  ()
*)

  (*
let _ = 
  let trailer = 
    if !trailer <> "" then !trailer else
      Filename.concat !confdir "Makefile.trailer" in
  let s = string_of_file trailer in
  output_string make_oc s
    *)
  
  
  
  

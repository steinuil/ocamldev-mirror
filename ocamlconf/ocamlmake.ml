(* 
BUGS:
 * all seen files should be loaded everytime
 * .mli files are not copied in tarballs
* assertion at line 737 should be replaced by message 
* dependencies towards configuration scripts


TODO:
1) proposition:
Instead of rebuilding all the dependencies when one ocamlconf.ocf has been
modified, generate a file Makefile.after, which contains only the dependencies
to be rebuilt. The rule that calls ocamlconf is then followed with
$(MAKE) -f Makefile.after depend

The only packages whose dependencies should be rebuilt are those depending
  on the library.
2) add dependencies toto.depend: .../ocamlconf.ocf   
*)

open Genlex
open Unix
open Ocamlconf_sort
open Ocamlconf_config
  
let make_oc = open_out "Makefile.rules"
let make2_oc = open_out "Makefile.depends"
let make3_oc = open_out "Makefile"

  
let _ =
  List.iter (fun rule_name ->
      Printf.fprintf make3_oc "%s:\n\t$(MAKE) -f Makefile.depends %s\n"
      rule_name rule_name
  ) [
    "all"; "byte"; "opt"; "depend"
  ];
  List.iter (fun rule_name ->
      Printf.fprintf make3_oc "%s:\n\t$(MAKE) -f Makefile.rules %s\n"
      rule_name rule_name
  ) [
    "clean"; "distclean"; "doc"
  ];
  close_out make3_oc
  
let  print_project pj =
  let name = pj.pj_libname in
  Printf.fprintf make_oc "\n#####################################\n";
  Printf.fprintf make_oc "# Project %s\n" name;
  Printf.fprintf make_oc "# From: %s\n" pj.pj_dirname;
  Printf.fprintf make_oc "# Score: %d\n" pj.pj_score;
  Printf.fprintf make_oc "# Enabled: %b\n" pj.pj_enabled;
  Printf.fprintf make_oc "# External deps: ";
  List.iter (fun dep -> Printf.fprintf make_oc " %s" dep) pj.pj_requires;
  Printf.fprintf make_oc "\n";
  Printf.fprintf make_oc "# Internal deps: ";
  Array.iteri (fun i b ->
      if b then Printf.fprintf make_oc " %s" projects.(i).pj_libname
  ) pj.pj_deps;
  Printf.fprintf make_oc "\n"

let _ = 
  Array.iter print_project all_projects

let _ = 
  Printf.fprintf make_oc "\n\nOCAMLCONF=%s\n" bin_name;
  Printf.fprintf make_oc "all:: Makefile %sbyte\n\n" (if !only_bytecode then "" else "opt ");
  
(*  Printf.fprintf make_oc "Makefile: $(OCAMLCONF)\n\t$(OCAMLCONF)\n\n"; *)
  
  Printf.fprintf make_oc "byte::\n";
  Printf.fprintf make_oc "opt::\n";
  Printf.fprintf make_oc "dependencies::\n";
(*  Printf.fprintf make_oc "tarball::\n"; *)
  Printf.fprintf make_oc "install::\n";
  Printf.fprintf make_oc "doc::\n";
  Printf.fprintf make_oc "\tmkdir -p doc/man doc/html\n";

(*  Printf.fprintf make_oc "tarballs:\n\tmkdir tarballs\n"; *)
  Printf.fprintf make_oc "include Makefile.config\n"; 
  ()
  
let rules = ref []
let need_rule_for file =
  if List.mem file !rules then false else begin
      rules := file :: !rules;
      true
    end
  
let modname file =
  let basename = Filename.basename file in
  basename.[0] <- Char.uppercase basename.[0];
  basename
  
let print_header pj =
  let name = pj.pj_libname in
  Printf.fprintf make_oc "\n\n#####################################\n";
  Printf.fprintf make_oc "# Project %s\n# From: %s\n\n" name pj.pj_dirname;
  Printf.fprintf make_oc "%s_CONF= " name;
  List.iter (fun name -> 
      Printf.fprintf make_oc " %s" name
  ) (List.rev pj.pj_included);
  Printf.fprintf make_oc " %s\n" pj.pj_rules;
  
  Printf.fprintf make_oc "Makefile: $(%s_CONF)\n" name;
  Printf.fprintf make_oc "\n%s_FILES=" name;      
  List.iter (fun oldfile ->
      let oldfile = Filename.concat pj.pj_dirname oldfile in
      Printf.fprintf make_oc " %s" (simplify_filename oldfile);
  ) pj.pj_files;
  Printf.fprintf make_oc "\n";
  
      
  Printf.fprintf make_oc "%s_CONFDEP = " name;
  Array.iteri (fun i b -> 
      let pj = projects.(i) in
      let name = pj.pj_libname in
      if b then Printf.fprintf make_oc "$(%s_CONF) " name
  ) pj.pj_deps;
  Printf.fprintf make_oc "\n";

  ()

let output_requires name pj =
  
  Printf.fprintf make_oc "%s_CMASDEP = " name;
  Array.iteri (fun i b -> 
      let pj = projects.(i) in
      let name = pj.pj_libname in
      if b then Printf.fprintf make_oc "$(%s_CMADEP) " name
  ) pj.pj_deps;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_CMAS = " name;
  List.iter (fun name -> 
      if Hashtbl.mem projects_by_name name then
        Printf.fprintf make_oc "$(%s_CMA) " name
      else
        Printf.fprintf make_oc "%s.cma " name
  ) pj.pj_requires;
  Printf.fprintf make_oc "$(%s_CMASDEP)\n" name;
  
  Printf.fprintf make_oc "%s_CMXASDEP = " name;
  Array.iteri (fun i b -> 
      let pj = projects.(i) in
      let name = pj.pj_libname in
      if b then Printf.fprintf make_oc "$(%s_CMXADEP) " name
  ) pj.pj_deps;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_CMXAS = " name;
  List.iter (fun name -> 
      if Hashtbl.mem projects_by_name name then
        Printf.fprintf make_oc "$(%s_CMXA) " name
      else
        Printf.fprintf make_oc "%s.cmxa " name) pj.pj_requires;
  Printf.fprintf make_oc "$(%s_CMXASDEP)\n" name;
 ()  
  
let build_project pj =
  let name = pj.pj_libname in
  
  let ml_files = ref [] in
  let mli_files = ref [] in
  let o_files = ref [] in
  let to_generate = ref [] in
  
  if pj.pj_type = PREPROCESSOR then begin
      Printf.fprintf make_oc "%s_PP = -I +camlp4 -pp \"camlp4o pa_op.cmo pa_ocamlconf.cmo q_MLast.cmo pa_extend.cmo pr_dump.cmo\" \n" name;
      Printf.fprintf make_oc "%s_PPDEP = " name;
      Array.iteri (fun i b -> 
          let pj = projects.(i) in
          let name = pj.pj_libname in
          if b then
            if !only_bytecode then
              Printf.fprintf make_oc "$(%s_CMADEP) " name
            else
              Printf.fprintf make_oc "$(%s_CMXADEP) " name
      ) pj.pj_deps;
      Printf.fprintf make_oc "\n";
    end
  else
  if pj.pj_pp <> "" then 
    begin
      let pp = pj.pj_pp in
      try
        let pj = Hashtbl.find projects_by_name pp in
        Printf.fprintf make_oc "%s_PP = -pp %s/mlpp%s\n" name pj.pj_dirname pp;
        Printf.fprintf make_oc "%s_PPDEP = %s/mlpp%s\n" name pj.pj_dirname pp;
      with _ ->
          Printf.fprintf make_oc "%s_PP = -pp %s\n" name pp;
    end;  
  
  let includes = ref [] in
  List.iter (fun r -> 
      let dir = Filename.concat ocamllib r in
      if is_directory dir then
        let dir = "+" ^ r in
        if not (List.mem dir !includes) then
          includes := dir :: !includes
  ) pj.pj_requires;
  Array.iteri (fun i b ->
      if b then
        let dir = projects.(i).pj_dirname in
        if not (List.mem dir !includes) then
          includes := dir :: !includes
  ) pj.pj_deps;
  
  Printf.fprintf make_oc "%s_INCLUDES= -I %s " name pj.pj_dirname;
  List.iter (fun dir -> Printf.fprintf make_oc "-I %s " dir) !includes;          
  Printf.fprintf make_oc "\n";
  
  let mli_rules file =
    
    if not (List.mem file !mli_files) then begin
        mli_files := file :: !mli_files;

        if need_rule_for (file ^ ".cmi") then begin
            
            Printf.fprintf make_oc "%s.cmi: %s.mli $(%s_PPDEP)\n" file file name;
            Printf.fprintf make_oc "\t$(OCAMLC) $(%s_INCLUDES) -c $(%s_PP) %s.mli\n" name name file;        
          end
      end
  
  in
  
  let ml_rules file = 
    
    if not pj.pj_installed  then
      let _ = () in
      
      ml_files := file :: !ml_files;
      
      if file.[0] <> '/' then begin
          if Sys.file_exists (Printf.sprintf "%s.mli" file) then 
            mli_rules file;
          
          if need_rule_for (file ^ ".cmo") then begin
              Printf.fprintf make_oc "%s.cmo: %s.ml $(%s_PPDEP)\n" file file name;
              Printf.fprintf make_oc "\t$(OCAMLC) $(%s_INCLUDES) -c $(%s_PP) %s.ml\n" name name file;
              
              Printf.fprintf make_oc "%s.cmx: %s.ml $(%s_PPDEP)\n" file file name;
              Printf.fprintf make_oc "\t$(OCAMLOPT) $(%s_INCLUDES) -c $(%s_PP) %s.ml\n" name name file;
          
              if pj.pj_pp <> "" then begin                  
                  Printf.fprintf make_oc "%s.mlt: %s.ml $(%s_PPDEP)\n" file file name;
                  Printf.fprintf make_oc "\t$(%s_PPDEP) -ml %s.ml > %s.mlt\n" name file file;
                end;
            end;          
        end  
  
  in
  
  List.iter (fun oldfile ->
      let oldfile = 
        if oldfile.[0] = '/' then
          Filename.concat ocamllib oldfile
        else
          Filename.concat pj.pj_dirname oldfile in
      let len = String.length oldfile in
      try
        let pos = String.rindex oldfile '.' in
        let ext = String.sub oldfile (pos+1) (len-pos-1) in
        let file = String.sub oldfile 0 pos in
        match ext with
        | "ml" -> 
            ml_rules file;
        
        | "mli" ->
            mli_rules file
        
        | "mll" ->
            mli_rules file;
            ml_rules file;
            to_generate := (file ^ ".ml") :: !to_generate;
        
        | "mly" ->
            mli_rules file;
            ml_rules file;
            to_generate := (file ^ ".ml") :: (file ^ ".mli") :: !to_generate;
        
        
        | "c" ->
            o_files := file :: !o_files
        | _ -> failwith "Unknown extension"
      with _ ->
          warning (
            Printf.sprintf "Cannot determine extension of '%s'" oldfile)
  ) pj.pj_files;
  
  let ml_files = List.rev !ml_files in
  let mli_files = List.rev !mli_files in
  let o_files = List.rev !o_files in
  let to_generate = List.rev !to_generate in

  let relative_file file = file.[0] <> '/' in
  
  Printf.fprintf make_oc "%s_DIR = %s\n" name pj.pj_dirname;
  
  Printf.fprintf make_oc "%s_CMOS =" name;
  List.iter (fun file -> 
        Printf.fprintf make_oc " %s.cmo" file
  ) ml_files;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_CMXS =" name;
  List.iter (fun file -> 
      Printf.fprintf make_oc " %s.cmx" file
  ) ml_files;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_MLS =" name;
  List.iter (fun file -> 
      if relative_file file then
        Printf.fprintf make_oc " %s.ml" file) ml_files;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_ANNOTS =" name;
  List.iter (fun file -> 
      if relative_file file then
        Printf.fprintf make_oc " %s.annot" file) ml_files;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_MLIS =" name;
  List.iter (fun file -> Printf.fprintf make_oc " %s.mli" file) mli_files;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_CMIS =" name;
  if pj.pj_interfaces = "" then 
    List.iter (fun file -> 
      if relative_file file then
        Printf.fprintf make_oc " %s.cmi" file) ml_files
  else
    Printf.fprintf make_oc "%s" pj.pj_interfaces;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_TMPS =" name;
  List.iter (fun file -> 
      if relative_file file then
        Printf.fprintf make_oc " %s.cmo %s.cmx %s.cmi %s.o" file file file file) ml_files;
  Printf.fprintf make_oc "\n";
  
  Printf.fprintf make_oc "%s_GENS =" name;
  List.iter (fun file -> Printf.fprintf make_oc " %s" file) to_generate;
  Printf.fprintf make_oc "\n";
  
  
  if not pj.pj_installed then
    
    let c_code = o_files <> [] || pj.pj_cclib <> "" in
    
    if c_code then begin
        
        Printf.fprintf make_oc "%s_CCLIB = -cclib \"%s" name pj.pj_cclib ;
        if o_files <> [] then
          Printf.fprintf make_oc " -L$(%s_DIR) -lml%s  -L$(%s_DIR)" 
          name name name;
        Printf.fprintf make_oc "\"\n";
        Printf.fprintf make_oc "%s_OS =" name;
        List.iter (fun file -> Printf.fprintf make_oc " %s.o" file) o_files;
        Printf.fprintf make_oc "\n";
        
        Printf.fprintf make_oc "%s_CFLAGS= %s\n" name pj.pj_cflags;
        
        List.iter (fun file ->
            Printf.fprintf make_oc "%s.o: %s.c\n" file file;
            Printf.fprintf make_oc "\t$(CC) $(%s_INCLUDES) -I $(OCAMLDIR) $(%s_CFLAGS) -o %s.o -c %s.c\n" name name file file;
        ) o_files;
      
      end;
    
    Printf.fprintf make_oc "%s.depend:: Makefile\n" name;
    Printf.fprintf make_oc "\tmv -f .%s_depend .%s_depend_old\n" name name;
    Printf.fprintf make_oc "\t$(MAKE) -f Makefile.rules .%s_depend || echo OK\n" name  ;
    Printf.fprintf make_oc ".%s_depend: $(%s_GENS) $(%s_PPDEP) $(%s_CONF) $(%s_CONFDEP)\n" name name name name name;
    Printf.fprintf make_oc "\t$(OCAMLDEP) $(%s_PP) $(%s_INCLUDES) $(%s_MLS) $(%s_MLIS) > .%s_depend_new\n" name name name name name ;
    Printf.fprintf make_oc "\tcmp .%s_depend_new .%s_depend_old || (rm -f .%s_depend_old; mv .%s_depend_new .%s_depend_old)\n" name name name name name ;
    Printf.fprintf make_oc "\tmv .%s_depend_old .%s_depend\n" name name ;
    Printf.fprintf make_oc "depend:: %s.depend\n" name ;
    Printf.fprintf make2_oc "-include .%s_depend\n" name ;
    
    Printf.fprintf make_oc "%s.distclean::\n" name;
    Printf.fprintf make_oc "\trm -f .%s_depend $(%s_GENS)\n" name name;
    
    begin  
      match pj.pj_type with
        LIBRARY -> 
          if ml_files <> [] then begin
              
              Printf.fprintf make_oc "%s_CMXADEP= $(%s_DIR)/%s.cmxa\n" name name name;
              Printf.fprintf make_oc "%s_CMXA= %s.cmxa\n" name name ;
              Printf.fprintf make_oc "%s_CMADEP= $(%s_DIR)/%s.cma\n" name name name;
              Printf.fprintf make_oc "%s_CMA= %s.cma\n" name name;

(* Bytecode first *)
              Printf.fprintf make_oc "$(%s_DIR)/%s.cma: .%s_depend $(%s_CMOS) $(%s_CONF)\n" name name name name name;
              Printf.fprintf make_oc "\t$(OCAMLC) -a $(%s_CCLIB) -o $(%s_DIR)/%s.cma $(%s_CMOS)\n" name name name name;
              
              Printf.fprintf make_oc "$(%s_DIR)/%s.cmxa: .%s_depend $(%s_CMXS) $(%s_CONF)\n" name name name name name;
              Printf.fprintf make_oc "\t$(OCAMLOPT) -a $(%s_CCLIB) -o $(%s_DIR)/%s.cmxa $(%s_CMXS)\n" name name name name;
              Printf.fprintf make_oc "%s.opt:: $(%s_DIR)/%s.cmxa\n" name name name;
              Printf.fprintf make_oc "%s.byte:: $(%s_DIR)/%s.cma\n" name name name;
              if not pj.pj_internal then begin
                  Printf.fprintf make_oc "%s.install::\n" name;
                  Printf.fprintf make_oc "\t$(CP) $(%s_DIR)/%s.cma $(%s_DIR)/%s.cmxa $(%s_DIR)/%s.a $(ALTLIB)\n" name name name name name name;
                  Printf.fprintf make_oc "\techo \"installed = true\" > $(ALTLIB)/%s.ocf\n" name ;
                  Printf.fprintf make_oc "\tcat $(%s_CONF) >> $(ALTLIB)/%s.ocf\n" name name;
                  Printf.fprintf make_oc "\t$(CP) $(%s_CMXS) $(ALTLIB)\n" name;
                  Printf.fprintf make_oc "\t$(CP) $(%s_CMIS) $(ALTLIB)\n" name;
                end;
              Printf.fprintf make_oc "%s.clean::\n" name;
              Printf.fprintf make_oc "\trm -f $(%s_DIR)/%s.cma $(%s_DIR)/%s.cmxa $(%s_DIR)/%s.a\n" name name name name name name;
              Printf.fprintf make_oc "\trm -f $(%s_OS) $(%s_TMPS) $(%s_ANNOTS)\n" name name name ;

              Printf.fprintf make_oc "$(HTML_DIR)/%s/index.html: $(%s_MLS)\n" name name;
              Printf.fprintf make_oc "\tmkdir -p $(HTML_DIR)/%s\n" name;
              Printf.fprintf make_oc "\t$(OCAMLDOC) -html -d $(HTML_DIR)/%s $(%s_INCLUDES) $(%s_PP) $(%s_MLS)\n" name name name name;
              Printf.fprintf make_oc "\t$(OCAMLDOC) -man -d $(MAN_DIR) $(%s_INCLUDES) $(%s_PP) $(%s_MLS)\n" name name name;
              
              
              Printf.fprintf make_oc "%s.doc:: $(HTML_DIR)/%s/index.html\n" name name;
              Printf.fprintf make_oc "doc:: %s.doc\n" name;
              
            end;

(* C code *)
          if c_code then begin
              Printf.fprintf make_oc "%s.byte:: $(%s_DIR)/libml%s.a\n" name name name;
              Printf.fprintf make_oc "%s.opt:: $(%s_DIR)/libml%s.a\n" name name name;
              if ml_files <> [] then begin
                  Printf.fprintf make_oc "$(%s_DIR)/%s.cma: $(%s_DIR)/libml%s.a\n" name name name name;
                  Printf.fprintf make_oc "$(%s_DIR)/%s.cmxa: $(%s_DIR)/libml%s.a\n" name name name name;
                end;
              Printf.fprintf make_oc "%s.clean::\n\trm -f $(%s_DIR)/libml%s.a\n" name name name;
              if not pj.pj_internal then begin
                  Printf.fprintf make_oc "%s.install::\n\t$(CP) $(%s_DIR)/libml%s.a $(ALTLIB)\n" name name name;
                end;
              Printf.fprintf make_oc "$(%s_DIR)/libml%s.a: $(%s_OS) $(%s_CONF)\n" name name name name;
              Printf.fprintf make_oc "\t$(AR) $(%s_DIR)/libml%s.a $(%s_OS)\n" name name name;
              Printf.fprintf make_oc "\t$(RANLIB) $(%s_DIR)/libml%s.a\n" name name;
            
            end;
                
      | PROGRAM -> 

          output_requires name pj;
          
          
          Printf.fprintf make_oc "$(%s_DIR)/%s.byte: .%s_depend $(%s_CMOS) $(%s_CMASDEP) $(%s_CONF)\n" name name name name name name;
          Printf.fprintf make_oc "\t$(OCAMLC) -custom  $(%s_INCLUDES) -o $(%s_DIR)/%s.byte  $(%s_CCLIB) $(%s_CMAS) $(%s_CMOS)\n" name name name name name name;
          
          Printf.fprintf make_oc "$(%s_DIR)/%s: .%s_depend $(%s_CMXS) $(%s_CMXASDEP) $(%s_CONF)\n" name name name name name name;
          Printf.fprintf make_oc "\t$(OCAMLOPT) -o $(%s_DIR)/%s $(%s_INCLUDES) $(%s_CCLIB) $(%s_CMXAS) $(%s_CMXS)\n" name name name name name name;
          
          Printf.fprintf make_oc "%s.clean::\n" name;
          Printf.fprintf make_oc "\trm -f $(%s_DIR)/%s.byte $(%s_DIR)/%s\n" name name name name;
          Printf.fprintf make_oc "\trm -f $(%s_OS) $(%s_TMPS)\n" name name ;
          
          Printf.fprintf make_oc "%s.byte:: $(%s_DIR)/%s.byte\n" name name name;
          Printf.fprintf make_oc "%s.opt:: $(%s_DIR)/%s\n" name name name;
          
          Printf.fprintf make_oc "byte:: %s.byte\n" name;
          Printf.fprintf make_oc "opt:: %s.opt\n" name;
          Printf.fprintf make_oc "clean:: %s.clean\n" name;
          Printf.fprintf make_oc "%s.doc:: $(%s_HTMLS)\n" name name;
          Printf.fprintf make_oc "doc:: %s.doc\n" name;

      
      | PREPROCESSOR ->
          
          output_requires name pj;

          if !only_bytecode then begin
              Printf.fprintf make_oc "$(%s_DIR)/mlpp%s: .%s_depend $(%s_CMOS) $(%s_CMASDEP) $(%s_CONF)\n" name name name name name name;
              Printf.fprintf make_oc "\t$(OCAMLC) -linkall -custom -o $(%s_DIR)/mlpp%s -I +camlp4 $(%s_CMAS) odyl.cma camlp4.cma $(%s_CMOS) odyl.cmo\n" name name name name;              
            end else begin
              Printf.fprintf make_oc "$(%s_DIR)/mlpp%s: .%s_depend $(%s_CMXS) $(%s_CMXASDEP) $(%s_CONF)\n" name name name name name name;
              Printf.fprintf make_oc "\t$(OCAMLOPT) -linkall -o $(%s_DIR)/mlpp%s -I +camlp4 $(%s_CMXAS) odyl.cmxa camlp4.cmxa $(%s_CMXS) odyl.cmx\n" name name name name;
            end;

(*
          Printf.fprintf make_oc "$(%s_DIR)/mlpp%s2ml: $(%s_CMXS)\n" name name name;
Printf.fprintf make_oc "\t$(OCAMLOPT) -linkall -o $(%s_DIR)/mlpp%s2ml -I +camlp4 odyl.cmxa camlp4.cmxa pa_o.cmx pa_op.cmx pa_macro.cmx  $(%s_CMXS) pr_o.cmx odyl.cmx\n" name name name;
*)
          Printf.fprintf make_oc "%s.clean::\n" name;
          Printf.fprintf make_oc "\trm -f $(%s_DIR)/mlpp%s $(%s_DIR)/mlpp%s2ml\n" name name name name;
          Printf.fprintf make_oc "\trm -f $(%s_TMPS)\n" name;
          
          Printf.fprintf make_oc "%s.opt::\n" name;
          Printf.fprintf make_oc "%s.byte::\n" name;
          Printf.fprintf make_oc "%s.install::\n" name;
(*          Printf.fprintf make_oc "%s.tarball::\n" name; *)
          
      | CONFIGURE -> ()          
      | PACKAGE -> ()
    end 
    

let build_project pj =
  let name = pj.pj_libname in
  print_header pj;
  
  begin  
    match pj.pj_type with
      PACKAGE ->
        
        
        Printf.printf "PACKAGE %s\n" name;                
        Printf.fprintf make_oc "%s.install::" name;
        List.iter (fun name ->
            Printf.fprintf make_oc " %s.install" name;
        ) pj.pj_files;
        Printf.fprintf make_oc "\n";
        
        Printf.fprintf make_oc "%s.byte::" name;
        List.iter (fun name ->
            Printf.fprintf make_oc " %s.byte" name;
        ) pj.pj_files;
        Printf.fprintf make_oc "\n";
        
        Printf.fprintf make_oc "%s.opt:: " name;
        List.iter (fun name ->
            Printf.fprintf make_oc " %s.opt" name;
        ) pj.pj_files;
        Printf.fprintf make_oc "\n";
        
        Printf.fprintf make_oc "%s.clean::" name;
        List.iter (fun name ->
            Printf.fprintf make_oc " %s.clean" name;
        ) pj.pj_files;
        Printf.fprintf make_oc "\n";
        
        Printf.fprintf make_oc "%s.distclean::" name;
        List.iter (fun name ->
            Printf.fprintf make_oc " %s.distclean" name;
        ) pj.pj_files;
        Printf.fprintf make_oc "\n";
                
        (*
        Printf.fprintf make_oc "%s_VERSION=%s\n" name pj.pj_version;
        Printf.fprintf make_oc "%s.tarball:: tarballs\n" name;
        Printf.fprintf make_oc "\trm -rf tarballs/%s-$(%s_VERSION) tarballs/%s-$(%s_VERSION).tar tarballs/%s-$(%s_VERSION).tar.gz\n" name name name name name name;
        Printf.fprintf make_oc "\tmkdir tarballs/%s-$(%s_VERSION)\n" name name;
        List.iter (fun pjname ->
            try
              ignore ( Hashtbl.find projects_by_name pjname );
              Printf.fprintf make_oc "\ttar cf tarballs/tmp.tar $(%s_FILES) $(%s_CONF)\n" pjname pjname;
              Printf.fprintf make_oc "\tcd tarballs/%s-$(%s_VERSION); tar xf ../tmp.tar\n" name name ;            
              Printf.fprintf make_oc "\trm -f tarballs/tmp.tar\n";
            with Not_found ->
                exit2 (Printf.sprintf "package %s: no project %s" name pjname)
        ) pj.pj_files;
        Printf.fprintf make_oc "\tcd tarballs; tar cf %s-$(%s_VERSION).tar %s-$(%s_VERSION) \n" name name name name;
        Printf.fprintf make_oc "\trm -rf tarballs/%s-$(%s_VERSION)\n" name name;
        Printf.fprintf make_oc "\tgzip tarballs/%s-$(%s_VERSION).tar\n" name name;
*)
        
    
    | CONFIGURE ->
                
        Printf.fprintf make_oc "%s.distclean::\n" name;
        Printf.fprintf make_oc "\trm -f ";
        List.iter (fun name -> Printf.fprintf make_oc "%s " (
              simplify_filename (Filename.concat pj.pj_dirname name))) pj.pj_generated;
        Printf.fprintf make_oc "\n";
                
    | _ -> build_project pj
  end;
  
  Printf.fprintf make_oc "byte:: %s.byte\n" name;
  Printf.fprintf make_oc "%s.byte::\n" name;
  
  Printf.fprintf make_oc "opt:: %s.opt\n" name;
  Printf.fprintf make_oc "%s.opt::\n" name;

  Printf.fprintf make_oc "clean:: %s.clean\n" name;
  Printf.fprintf make_oc "%s.clean::\n" name;
  
  Printf.fprintf make_oc "distclean:: %s.clean %s.distclean\n" name name;
  Printf.fprintf make_oc "%s.distclean:: %s.clean\n" name name;
  
  Printf.fprintf make_oc "install:: %s.install\n" name;
  Printf.fprintf make_oc "%s.install::\n" name;
  
  (*
  Printf.fprintf make_oc "tarball:: %s.tarball\n" name;
  Printf.fprintf make_oc "%s.tarball::\n" name;
*)
  
  ()


let _ =   Array.iter (fun pj ->
      if pj.pj_enabled then build_project pj) all_projects
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

let  _ =
  
  Printf.fprintf make_oc "\nMakefile:\n\t$(OCAMLCONF)\n\n"; 

  Printf.fprintf make_oc ".SUFFIXES: .mli .mll .mly .ml .cc .mlii\n";
  Printf.fprintf make_oc "\n";
  Printf.fprintf make_oc ".ml.mlii :\n";
  Printf.fprintf make_oc "	rm -f $*.mli\n";
  Printf.fprintf make_oc "	$(OCAMLC) -i $(OFLAGS) $(INCLUDES) -c $< > $*.mlii\n";
  Printf.fprintf make_oc "	mv $*.mlii $*.mli\n";
  Printf.fprintf make_oc ".mll.ml :\n";
  Printf.fprintf make_oc "	$(OCAMLLEX) $<\n";
  Printf.fprintf make_oc "\n";
  Printf.fprintf make_oc ".mly.ml :\n";
  Printf.fprintf make_oc "	$(OCAMLYACC) $<\n";
  Printf.fprintf make_oc "\n";
  Printf.fprintf make_oc ".mly.mli:\n";
  Printf.fprintf make_oc "	$(OCAMLYACC) $<\n";
  ()
  
let _ =
  close_out make_oc;
  
  
  Printf.fprintf make2_oc "include Makefile.rules\n";
  close_out make2_oc;
  
  let state = {
      state_configures = ref [];
      state_devel_projects = ref [];
      state_installed_projects = ref [];
    } in
  Array.iter (fun pj ->
      if pj.pj_installed then
        state.state_installed_projects := pj.pj_rules :: !(state.state_installed_projects)
      else
        state.state_devel_projects := pj.pj_rules :: !(state.state_devel_projects)
  ) all_projects;
(*  let state_oc = open_out state_filename in
  output_value state_oc state;
  close_out state_oc; *)

  let b = Buffer.create 1000 in
  
  Printf.bprintf b "ocamlver = \"%s\"\n" ocamlver;
  Array.iter (fun pj ->
      Printf.bprintf b "%s = %b\n" pj.pj_libname pj.pj_enabled;
  ) all_projects;
  let s1 = Buffer.contents b in
  
  let save =
    if Sys.file_exists "ocamlconf.res" then
      let s2 = string_of_file "ocamlconf.res" in
      s1 <> s2
    else
      true
  in
  if save then
    file_of_string "ocamlconf.res" s1
  
  
  
  


List.iter (fun s ->
    if Fnmatch.fnmatch "*.ml" s [] then 
      (Printf.printf "[%s]" s; print_newline ();))
["toto.mli";
  "otto";
  "test.ml";
  "encore.ml";
  "roto"]
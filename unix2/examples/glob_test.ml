
Array.iter (fun s ->
    Printf.printf "[%s]" s; print_newline ();)
(Glob.glob "*.ml" [])
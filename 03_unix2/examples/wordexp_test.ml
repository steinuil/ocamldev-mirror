
Array.iter (fun s ->
    Printf.printf "[%s]" s; print_newline ();)
(Wordexp.wordexp "~/*.ml = $HOME/*.ml" [])
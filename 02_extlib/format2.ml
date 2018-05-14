let redirect_to_string action arg =
  let string = Buffer.create 10 in
  let (p,f) = Format.get_formatter_output_functions () in
  Format.set_formatter_output_functions 
    (fun str pos len ->
      let s = String.sub str pos len in
      Buffer.add_string string s) Pervasives2.unit;
  try  
    action arg;
    Format.print_flush ();
    Format.set_formatter_output_functions p f;
    Buffer.contents string
  with
    e -> 
      Format.print_flush ();
      Format.set_formatter_output_functions p f;
      raise e

         
let formatter_of_out_channel oc =
  Format.make_formatter (output oc) (fun _ -> flush oc)

let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
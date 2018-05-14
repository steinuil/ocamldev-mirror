open Options
  open Utils
  open Interactive
  open Efuns
  open Keymap
  open Simple
  open Select
  open Search
  open Eval
  open Compil
  open Abbrevs
  open Complex
  open System
  open Multi_frames
  open Top_window
  
  
(* Registres *)
  
module Register = struct

let max = 10
  
let code_one = 49
  
let registers = Array.create max ""
  
let put_in_register n frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark =
    match buf.buf_mark with
      None -> failwith "No mark set"
    | Some mark -> mark
  in
  let (start,term) = 
    if mark > point then (point,mark) else (mark,point)
  in
  let _,region = Text.copy_res text start (Text.distance text start term) in
  Array.set registers n region
  
let paste_register n frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let pos, len =  Text.insert_res text point (Array.get registers n) in
  Text.fmove text point len; 
  last_insert := Some(frame,pos,0,len)
  
(* Generic command, allowing to choose a register in the minibuffer *)
  
let make_binding_put region frame map n = 
  Keymap.add_binding map [NormalMap, code_one + n ] 
    (fun mini_frame ->
      Minibuffer.kill mini_frame frame;
      Array.set registers n region)
      
let make_bindings_put region frame map =
  for i = 0 to (max - 1) do
    make_binding_put region frame map i
  done
  
let put_in_a_register frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark =
    match buf.buf_mark with
      None -> failwith "No mark set"
    | Some mark -> mark
  in
  let (start,term) = 
    if mark > point then (point,mark) else (mark,point)
  in
  let _,region = Text.copy_res text start (Text.distance text start term) in
  let map = Keymap.create () in
  let request = Printf.sprintf "Put selected text in register :"
  in
  make_bindings_put region frame map;
  let _ = Minibuffer.create frame map request in ()

(* Generic command, allowing to choose a register to paste *)  
  
let make_binding_paste frame map n = 
  Keymap.add_binding map [NormalMap, code_one + n ] 
    (fun mini_frame ->
      Minibuffer.kill mini_frame frame;
      let str = Array.get registers n in
      let buf = frame.frm_buffer in
      let text = buf.buf_text in
      let point = frame.frm_point in
      let pos, len =  Text.insert_res text point str in
      Text.fmove text point len; 
      last_insert := Some(frame,pos,0,len))

let make_bindings_paste frame map =
  for i = 0 to (max - 1) do
    make_binding_paste frame map i
  done
  
let paste_a_register frame = 
  let map = Keymap.create () in
  let request = Printf.sprintf "Paste content of register :"
  in
  make_bindings_paste frame map;
  let _ = Minibuffer.create frame map request in ()
  
  
end


(* To change to default buffer without asking the user *)
let change_to_default_buffer frame =
  let default = get_previous_frame () in
  set_previous_frame frame;
  let window = frame.frm_window in
  Frame.change_buffer window default

let _ = 
  define_action "copy_region" Simple.copy_region;
  define_action "change_to_default_buffer" change_to_default_buffer;
  define_action "put_in_F1" (Register.put_in_register 0) ;
  define_action "put_in_F2" (Register.put_in_register 1) ;
  define_action "put_in_F3" (Register.put_in_register 2) ;
  define_action "paste_F1" (Register.paste_register 0) ;
  define_action "paste_F2" (Register.paste_register 1) ;
  define_action "paste_F3" (Register.paste_register 2) ;
  define_action "put_in_a_register" (Register.put_in_a_register) ;
  define_action "paste_a_register" (Register.paste_a_register)
  
let my_map = define_option ["my_map"] "" 
    (list_option binding_option) 
  []
  
let c_x = (ControlMap, Char.code 'x') 
  
let _ = 
  if !!my_map = [] then begin
      my_map =:= [
        [NormalMap, XK.xk_F5], "next_frame" ;
        [NormalMap, XK.xk_F6], "change_to_default_buffer" ;
        [NormalMap, XK.xk_F9], "save_buffer" ;
        [NormalMap, XK.xk_F10], "compile" ;
        [NormalMap, XK.xk_End], "end_of_file";
        [NormalMap, XK.xk_Begin], "begin_of_file";
        [MetaMap, Char.code 'w'], "copy_region" ;
        [ControlMap, Char.code '/'], "undo" ;
        [ControlMap, XK.xk_Delete], "delete_backward_word" ;
        [ControlMap, XK.xk_BackSpace], "delete_backward_word";
        [ControlMap, Char.code '!'], "put_in_F1";
        [ControlMap, Char.code '@'], "put_in_F2";
        [ControlMap, Char.code '#'], "put_in_F3";
        [NormalMap, XK.xk_F1], "paste_F1";
        [NormalMap, XK.xk_F2], "paste_F2";
        [NormalMap, XK.xk_F3], "paste_F3";
        [c_x ; NormalMap, Char.code 'r' ; NormalMap, Char.code 's' ],
        "put_in_a_register";
        [c_x ; NormalMap, Char.code 'r' ; NormalMap, Char.code 'i' ],
        "paste_a_register"
        ]
  end
      
let init_my_map location = 
  
  List.iter (fun (keys, action) ->
      try
        Keymap.add_global_key location keys action (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  ) !!my_map
      
let _ =
  Efuns.add_start_hook (fun location ->
      add_option_parameter location compile_find_makefile;
      add_option_parameter location Text.add_amount;
      init_my_map location)

  
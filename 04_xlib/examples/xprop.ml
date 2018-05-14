(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*
    Le programme xprop en CAML
*)


open Xtypes
open Xlib;;

(* connectons nous au display pour commencer *)

let dpy = openDisplay ""
let root = defaultRoot dpy;;

let getSelection dpy win =
  Selection.getSelection dpy win
    XA.xa_primary XA.xa_string !Eloop.event_time 


let get_cutbuffer dpy window =
  try
    try
      getSelection dpy window
    with
      _ ->
        let prop = getWholeProperty dpy root 
            XA.xa_cut_buffer0 in
        if prop.gp_type = XA.xa_string then
          prop.gp_value
        else
          raise Not_found
  with e ->
      Printf.printf "Exception %s\n%!" (Utils.printexn e);
      exit 2
      
let setSelection dpy win string =
  Selection.setSelection dpy win XA.xa_primary
    (fun target ->
      if target = XA.xa_string then 1, string else raise Not_found)
  !Eloop.event_time

let removeSelection dpy =
  Selection.removeSelection dpy XA.xa_primary 
    !Eloop.event_time
  


let set_cutbuffer window str =
  X.rotateProperties dpy root 1 [
    XA.xa_cut_buffer0;
    XA.xa_cut_buffer1;
    XA.xa_cut_buffer2;
    XA.xa_cut_buffer3;
    XA.xa_cut_buffer4;
    XA.xa_cut_buffer5;
    XA.xa_cut_buffer6;
    XA.xa_cut_buffer7];
  X.changeProperty dpy root PropModeReplace
    XA.xa_cut_buffer0  XA.xa_string
    1 str;
  setSelection dpy window str
  

let _ =
  set_cutbuffer root "Bonjour";
  Printf.printf "CUTBUFFER: %s\n%!" (get_cutbuffer dpy root);
  flush stdout
  
  
(* un curseur + *)
let cursor = createFontCursor dpy  XC.xc_crosshair;;

(* xprop dpy;; pour utiliser *)
let xprop dpy =
  List.iter 
    (function (atom,typ,n1,n2,value) ->
       print_string (atom^"("^typ^")= "^
        (
	  if typ = "STRING" then value else ""
	));
       print_newline ()

    )

    (
    listProperties dpy (
    	let win=Xmu.selectWindow dpy root cursor in
	  if win = noWindow then root
	  else
            Xmu.clientWindow dpy win
      ))
 ;;

xprop dpy;;



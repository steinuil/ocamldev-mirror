(***************************************************************************)
(**                                                                       **)
(** This file was written by Alexandre Miquel (Alexandre.Miquel@inria.fr) **)
(**                                                                       **)
(**         You are allowed to use it, re-distribute it, modify           **)
(**           it and to re-distribute the modified versions.              **)
(**                                                                       **)
(***************************************************************************)
    

open Unix
open Concur
open ThreadUnix

let alpha = "abcdefghijklmnopqrstuvwxyz" ;;
let hex = "0123456789abcdef" ;;

exception Found of file_descr ;;

let plist = ref [] ;;

let open_process cmd =
  let master = String.copy "/dev/pty??" in
  (* recherche du pseudo-terminal *)
  let fd_front =
    try
      for i = 0 to String.length alpha - 1 do
	master.[8] <- alpha.[i] ; (* "/dev/pty??" -> "/dev/ptyX?" *)
	for j = 0 to String.length hex - 1 do
	  master.[9] <- hex.[j] ; (* "/dev/ptyX?" -> "/dev/ptyXY" *)
	  try
	    let fd = openfile master [O_RDWR] 0 in
	    raise (Found fd)
	  with
	  | Unix_error _ -> ()
	done
      done ;
      failwith "Pty.open_process"
    with
    | Found fd -> fd in
  (* Assignation du terminal esclave *)
  let slave = String.copy master in
  slave.[5] <- 't' ;
  match fork () with
  | 0 ->
      (* Processus fils *)
      let _ = setsid () in
      let fd_back = openfile slave [O_RDWR] 0 in
      dup2 fd_back stdin ;
      dup2 fd_back stdout ;
      dup2 fd_back stderr ;
      close fd_back ;
      close fd_front ;
      execv "/bin/sh" [| "sh"; "-c"; cmd |] ;
      (* On ne devrait jamais arriver ici! *) ;
      exit 2
  | pid ->
      (* processus père *)
      let fd_front' = dup fd_front in
      let ch_in = in_channel_of_descr fd_front
      and ch_out = out_channel_of_descr fd_front' in
      plist := ((ch_in,ch_out),pid)::(!plist) ;
      (ch_in,ch_out) ;;

let close_process ((ch_in,ch_out) as c) =
  try
    let pid = List.assoc c !plist in
    close_in ch_in ;
    close_out ch_out ;
    let (_,stat) = waitpid [] pid in
    plist := List.remove_assoc c !plist ;
    stat
  with
  | Not_found -> failwith "Pty.close_process" ;;

(***************************************************************************)
(**                                                                       **)
(** This file was written by Alexandre Miquel (Alexandre.Miquel@inria.fr) **)
(**                                                                       **)
(**         You are allowed to use it, re-distribute it, modify           **)
(**           it and to re-distribute the modified versions.              **)
(**                                                                       **)
(***************************************************************************)
    

val open_process : string -> in_channel * out_channel ;;
val close_process : (in_channel * out_channel) -> Unix.process_status ;;

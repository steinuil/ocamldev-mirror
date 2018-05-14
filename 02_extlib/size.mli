(*
 * Size: computes the memory size of an ocaml value.
 * Copyright (C) 2000 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* Sizes of ocaml values (in their memory representation). 
   Sizes are given in words ([size_w]) or in bytes ([size_b]), in a
   system-independent way. *)

val size_w : 'a -> int

val size_b : 'a -> int


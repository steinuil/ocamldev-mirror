(*
 * Trie: maps over lists.
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

(*s This module implements @emph{tries}. Given a map [M] over an
    arbitrary type [M.key], the following functor constructs a new map
    over type [M.key list]. *)

module Make(M : Map.S) : (Map.S with type key = M.key list)

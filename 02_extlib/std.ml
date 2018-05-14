(*
 * $Id: std.ml,v 1.2 2001/09/09 22:43:43 lefessan Exp $

    Copyright (c) 1999 Christian Lindig <lindig@ips.cs.tu-bs.de>. All
    rights reserved. See COPYING for details.
 *
 * This module provides small but useful functions which are not provided
 * by the OCaml standard library.
 *)


let (<-<) f g = fun x -> f (g x)

let (>->) f g = fun x -> g (f x) 


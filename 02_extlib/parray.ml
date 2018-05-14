(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parray.ml,v 1.2 2001/07/26 16:38:29 lefessan Exp $ *)

(* Array operations *)

type t = Nothing | Something of t

external length : t array -> int = "%array_length"
external get: t array -> int -> t = "%array_safe_get"
external set: t array -> int -> t -> unit = "%array_safe_set"
external unsafe_get: t array -> int -> t = "%array_unsafe_get"
external unsafe_set: t array -> int -> t -> unit = "%array_unsafe_set"
external make: int -> t -> t array = "make_vect"
external create: int -> t -> t array = "make_vect"

let to_t v = (Obj.magic v : t)
external check_ta : 'a array -> bool = "array_checkta" "noalloc"
external check_tv : 'a -> bool = "array_checktv" "noalloc"
let to_tv v = 
  assert (check_tv v);
  (Obj.magic v : t)
let to_ta v = 
  assert (check_ta v);
  (Obj.magic v : t array)
let of_ta v =
  (Obj.magic v : 'a array)
let of_tv v =
  (Obj.magic v : 'a)
  
let init l f =
  if l = 0 then [||] else
   let res = create l (f 0) in
   for i = 1 to pred l do
     unsafe_set res i (f i)
   done;
   res 

let make_matrix sx sy init =
  let res = create sx (to_t [||]) in
  for x = 0 to pred sx do
    unsafe_set res x (to_t (create sy init))
  done;
  res

let create_matrix = make_matrix

let copy a =
  let a = to_ta a in
  let l = length a in
  if l = 0 then [||] else begin
    let res = create l (unsafe_get a 0) in
    for i = 1 to pred l do
      unsafe_set res i (unsafe_get a i)
    done;
    of_ta res
  end

let append a1 a2 =
  let l1 = length a1 and l2 = length a2 in
  if l1 = 0 && l2 = 0 then [||] else begin
    let r = create (l1 + l2) (unsafe_get (if l1 > 0 then a1 else a2) 0) in
    for i = 0 to l1 - 1 do unsafe_set r i (unsafe_get a1 i) done;  
    for i = 0 to l2 - 1 do unsafe_set r (i + l1) (unsafe_get a2 i) done;  
    r
  end

let concat_aux init al =
  let rec size accu = function
    | [] -> accu
    | h::t -> let h = to_ta h in size (accu + length h) t
  in
  let res = create (size 0 al) init in
  let rec fill pos = function
    | [] -> ()
    | h::t -> 
        let h = to_ta h in
        for i = 0 to length h - 1 do
          unsafe_set res (pos + i) (unsafe_get h i);
        done;
        fill (pos + length h) t;
  in
  fill 0 al;
  res
;;

let concat al =
  let rec find_init = function
      [] -> [||]
    | a :: rem ->
        let a = to_ta a in
        if length a > 0 then concat_aux (unsafe_get a 0) al 
          else find_init rem
  in 
  of_ta (find_init al)

let sub a ofs len =
  let a = to_ta a in
  if ofs < 0 || len < 0 || ofs + len > length a then invalid_arg "Array.sub"
  else if len = 0 then [||]
  else begin
    let r = create len (unsafe_get a ofs) in
    for i = 1 to len - 1 do unsafe_set r i (unsafe_get a (ofs + i)) done;
    of_ta r
  end

let fill a ofs len v =
  let a = to_ta a in
  let v = to_tv v in
  if ofs < 0 || len < 0 || ofs + len > length a
  then invalid_arg "Array.fill"
  else for i = ofs to ofs + len - 1 do unsafe_set a i v done

let blit a1 ofs1 a2 ofs2 len =
  let a1 = to_ta a1 in
  let a2 = to_ta a2 in
  if len < 0 || ofs1 < 0 || ofs1 + len > length a1
             || ofs2 < 0 || ofs2 + len > length a2
  then invalid_arg "Array.blit"
  else if ofs1 < ofs2 then
    (* Top-down copy *)
    for i = len - 1 downto 0 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done
  else
    (* Bottom-up copy *)
    for i = 0 to len - 1 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done

let iter f a =
  let a = to_ta a in
  for i = 0 to length a - 1 do f(of_tv (unsafe_get a i)) done

let map f a =
  let a = to_ta a in
  let l = length a in
  if l = 0 then [||] else begin
    let r = Array.create l (f(of_tv (unsafe_get a 0))) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f(of_tv (unsafe_get a i)))
    done;
    r
  end

let iteri f a =
  let a = to_ta a in
  for i = 0 to length a - 1 do f i (of_tv (unsafe_get a i)) done

let mapi f a =
  let a = to_ta a in
  let l = length a in
  if l = 0 then [||] else begin
      let r = Array.create l (f 0 (of_tv (unsafe_get a 0))) in
      for i = 1 to l - 1 do
        Array.unsafe_set r i (f i (of_tv (unsafe_get a i)))
      done;
      of_ta r
  end

let to_list a =
  let a = to_ta a in
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (of_tv (unsafe_get a i) :: res) in
  tolist (length a - 1) []

let rec list_length accu = function
  | [] -> accu
  | h::t -> list_length (succ accu) t
;;

let of_list = function
    [] -> [||]
  | hd::tl as l ->
      let hd = to_tv hd in
      let a = create (list_length 0 l) hd in
      let rec fill i = function
          [] -> of_ta a
        | hd::tl -> 
            let hd = to_tv hd in
            unsafe_set a i hd; fill (i+1) tl in
      fill 1 tl

let fold_left f x a =
  let a = to_ta a in
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (of_tv (unsafe_get a i))
  done;
  !r

let fold_right f a x =
  let a = to_ta a in
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (of_tv (unsafe_get a i)) !r
  done;
  !r

exception Bottom of int;;
let sort cmp a =
  let a = to_ta a in
  let cmp = Obj.magic  a in
  let maxson l i =
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp (get a i31) (get a (i31+1)) < 0 then x := i31+1;
      if cmp (get a !x) (get a (i31+2)) < 0 then x := i31+2;
      !x
    end else
      if i31+1 < l && cmp (get a i31) (get a (i31+1)) < 0
      then i31+1
      else if i31 < l then i31 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp (get a j) e > 0 then begin
      set a i (get a j);
      trickledown l j e;
    end else begin
      set a i e;
    end;
  in
  let rec trickle l i e = try trickledown l i e with Bottom i -> set a i e in
  let rec bubbledown l i =
    let j = maxson l i in
    set a i (get a j);
    bubbledown l j;
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    assert (i <> father);
    if cmp (get a father) e < 0 then begin
      set a i (get a father);
      if father > 0 then trickleup father e else set a 0 e;
    end else begin
      set a i e;
    end;
  in
  let l = length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i (get a i); done;
  for i = l - 1 downto 2 do
    let e = (get a i) in
    set a i (get a 0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = (get a 1) in set a 1 (get a 0); set a 0 e);
;;

let cutoff = 5;;
let stable_sort cmp a =
  let a = to_ta a in
  let cmp = Obj.magic cmp in
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        set dst d s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (get a i1) i2 s2 (d + 1)
        else
          blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        set dst d s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (get src2 i2) (d + 1)
        else
          blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs (get a src1ofs) src2ofs (get src2 src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = (get a (srcofs + i)) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp (get dst !j) e > 0) do
        set dst (!j + 1) (get dst !j);
        decr j;
      done;
      set dst (!j + 1) e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = make l2 (get a 0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

external length : 'a array -> int = "%array_length"
external get: 'a array -> int -> 'a = "%array_safe_get"
external set: 'a array -> int -> 'a -> unit = "%array_safe_set"
let make len v =
  of_ta (make len (to_tv v))
let create len v = 
  of_ta (make len (to_tv v))
let init len f = of_ta (init len (fun i -> to_tv (f i)))
let make_matrix l1 l2 v =
  of_ta (make_matrix l1 l2 (to_tv v))
let create_matrix  l1 l2 v =
  of_ta (make_matrix l1 l2 (to_tv v))
let append t1 t2 = 
  of_ta (append (to_ta t1) (to_ta t2))
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
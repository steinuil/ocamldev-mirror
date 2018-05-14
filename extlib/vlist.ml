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

(* $Id: vlist.ml,v 1.2 2001/08/18 15:01:39 lefessan Exp $ *)

type 'a t = {
    len : int;
    list : 'a vlist;
  }
  
and 'a vlist = {
    mutable next : int;
    tab : 'a array;
  }

let nil () = { len = 0; list = { next = 1; tab = [||]; } }
let cons v tail = 
  let len = tail.len in
  let list = tail.list in
  let next = list.next in
  let tab = list.tab in
  let tlen = Array.length tab in
  let list = 
    if len = tlen then 
      let new_tab = Array.create (2 * tlen + 2) v in
      Array.blit tab 0 new_tab 0 tlen;
      { next = next + 1; tab = new_tab }
    else 
    if len < next then
      let new_tab = Array.create tlen v in
      Array.blit tab 0 new_tab 0 len;
      { next = len + 1; tab = new_tab }
    else 
      begin
        list.next <- next + 1;
        list.tab.(len) <- v;
        list
      end
  in
  { len = len+1; list = list }

  
let is_nil l = l.len = 0
  
let hd l = 
  let len = l.len in
  let list = l.list in
  let tab = list.tab in
  let next = list.next in
  if len = 0 then failwith "hd";
  tab.(len-1)
  
let tl l =
  let len = l.len in
  let list = l.list in
  let tab = list.tab in
  let next = list.next in
  if len = 0 then failwith "tl";
  { len = len - 1; list = list; }

let rec to_list_rec tab n len list =
  if n = len then list else
    to_list_rec tab (n+1) len (tab.(n) :: list)
    
let to_list l =
  let len = l.len in
  if len  = 0 then [] else
    to_list_rec l.list.tab 0 len []
  

let rec of_list l =
  match l with
    [] -> nil ()
  | v :: tl ->
      cons v (of_list tl)
  
(* List operations *)

let length l = l.len

let nth l n = 
  let len = l.len in
  let list = l.list in
  let tab = list.tab in
  if n < 0 then invalid_arg "Vlist.nth" 
  else if len < n then tab.(n) else failwith "nth"

let append l2 l1 =
  let len2 = l2.len in
  let list2 = l2.list in
  let tab2 = list2.tab in
  let len1 = l1.len in
  let list1 = l1.list in
  let next1 = list1.next in
  let tab1 = list1.tab in
  let len3 = len1+len2 in
  let tlen1 = Array.length tab1 in
  if len1 = 0 then l2 else
  if len2 = 0 then l1 else
  if len1 = next1 && len3 <= tlen1 then begin
      Array.blit tab2 0 tab1 len1 len2;
      list1.next <- len3;
      { len = len3; list = list1 }
    end else
  let new_len = len3 + 2 in
  let new_tab = Array.create new_len tab1.(0) in
  Array.blit tab1 1 new_tab 1 (len1 - 1);
  Array.blit tab2 0 new_tab len1 len2;
  { len = len3; list = { next = len3; tab = new_tab }}
  
let rev l = 
  let len = l.len in
  if len = 0 then l else
  let list = l.list in
  let tab = list.tab in
  
  let new_tab = Array.create len tab.(len-1) in
  for i = 1 to len-1 do
    new_tab.(i) <- tab.(len-i-1)
  done;
  { len = len; list = { next = len; tab = new_tab }}

let flatten l =
  let len = l.len in
  if len = 0 then nil () else 
  if len = 1 then l.list.tab.(0) else
  let list = l.list in
  let tab = list.tab in
  
  let rec iter n =
    if n = len then (0, nil ()) else
    let t = tab.(n) in
    let tlen = t.len in
    if tlen > 0 then iter_len (n+1) tlen t 
    else iter (n+1)
      
  and iter_len n new_len t =
    if n = len then (new_len, t) else
    let tlen = tab.(n).len in
    iter_len (n+1) (new_len + tlen) t 
      
  in
  let (new_len, t) = iter 0 in
  let tlen = t.len in
  if new_len = tlen then t else
  let new_tab = Array.create new_len t.list.tab.(0) in
  let rec iter n pos =
    if n < 0 then
      let t = tab.(n) in
      let tlen = t.len in
      if tlen = 0 then iter (n-1) pos else
      let tab = t.list.tab in
      Array.blit tab 0 new_tab pos tlen;
      iter (n-1) (pos+tlen)
  in
  iter (len-1) 0;
  { len = new_len; list = { next = new_len; tab = new_tab }}

let rev_append l1 l2 = append (rev l1) l2
  
let concat = flatten
  
let map f l =
  let len = l.len in
  if len = 0 then nil () else 
  let list = l.list in
  let tab = list.tab in
  let e = tab.(len-1) in
  let v = f e in
  let new_tab = Array.create len v in
  for i = len-2 downto 0 do
    new_tab.(i) <- f tab.(i)
  done;
  { len = len; list = { next = len; tab = new_tab }}
  
let rev_map f l =
  let len = l.len in
  if len = 0 then nil () else 
  let list = l.list in
  let tab = list.tab in
  let e = tab.(len-1) in
  let v = f e in
  let new_tab = Array.create len v in
  for i = len-2 downto 0 do
    new_tab.(len - i - 1) <- f tab.(i)
  done;
  { len = len; list = { next = len; tab = new_tab }}
  
let iter f l =
  let len = l.len in
  if len > 0 then  
    let list = l.list in
    let tab = list.tab in
    for i = len-1 downto 0 do
      f tab.(i)
    done

let rec fold_left_rec f tab accu n =
  if n < 0  then accu else
    fold_left_rec f tab (f accu tab.(n)) (n-1)
    
let fold_left f accu l =
  let len = l.len in
  if len > 0 then  
    let list = l.list in
    let tab = list.tab in
    fold_left_rec f tab accu (len-1)
  else accu

let rec fold_right_rec f tab len accu n =
  if n = len  then accu else
    fold_right_rec f tab len (f tab.(n) accu) (n+1)
    
let fold_right f l accu =
  let len = l.len in
  if len > 0 then  
    let list = l.list in
    let tab = list.tab in
    fold_right_rec f tab len accu 0
  else accu

let map2 f l1 l2 =
  let len1 = l1.len in
  let len2 = l2.len in
  if len1 <> len2 then invalid_arg  "Vlist.map2";
  if len1 = 0 then nil  () else
  let tab1 = l1.list.tab in
  let tab2 = l2.list.tab in
  let e1 = tab1.(len1 - 1) in
  let e2 = tab2.(len1-1) in
  let v = f e1 e2 in
  let new_tab = Array.create len1 v in
  for i = len1-2 downto 0 do
    new_tab.(len1 - i - 1) <- f tab1.(i) tab2.(i)
  done;
  { len = len1; list = { next = len1; tab = new_tab }}

let rev_map2 f l1 l2 =
  rev (map2 f l1 l2)

let iter2  f l1 l2 =
  let len1 = l1.len in
  let len2 = l2.len in
  if len1 <> len2 then invalid_arg  "Vlist.iter2";
  if len1 <> 0 then
    let tab1 = l1.list.tab in
    let tab2 = l2.list.tab in
    for i = len1-1 downto 0 do
      f tab1.(i) tab2.(i)
    done

    
let rec fold_left_rec2 f tab1 tab2 accu n =
  if n < 0  then accu else
    fold_left_rec2 f tab1 tab2 (f accu tab1.(n) tab2.(n)) (n-1)
    
let fold_left2 f accu l1 l2 =
  let len1 = l1.len in
  let len2 = l2.len in
  if len1 <> len2 then invalid_arg  "Vlist.map2";
  if len1 = 0 then accu else
  let tab1 = l1.list.tab in
  let tab2 = l2.list.tab in
  fold_left_rec2 f tab1 tab2 accu (len1-1)

let rec fold_right_rec2 f tab1 tab2 len accu n =
  if n = len  then accu else
    fold_right_rec2 f tab1 tab2 len (f tab1.(n) tab2.(n) accu) (n+1)
    
let fold_right2 f l1 l2 accu =
  let len1 = l1.len in
  let len2 = l2.len in
  if len1 <> len2 then invalid_arg  "Vlist.map2";
  if len1 = 0 then accu else
  let tab1 = l1.list.tab in
  let tab2 = l2.list.tab in
  fold_right_rec2 f tab1 tab2 len1 accu 0

let rec for_all_rec p tab n len=
  n = len  ||
  let x = tab.(n) in
  p x && for_all_rec p tab (n+1) len
  
let for_all p l =
  let len = l.len in
  len = 0 || 
  let list = l.list in
  let tab = list.tab in
  for_all_rec p tab 0 len

let rec exists_rec p tab n len=
  n < len  &&
  let x = tab.(n) in
  p x || exists_rec p tab (n+1) len
  
let exists p l =
  let len = l.len in
  len > 0 &&
  let list = l.list in
  let tab = list.tab in
  exists_rec p tab 0 len

(*

let rec for_all2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2 p l1 l2
  | (_, _) -> invalid_arg "List.for_all2"

let rec exists2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> false
  | (a1::l1, a2::l2) -> p a1 a2 || exists2 p l1 l2
  | (_, _) -> invalid_arg "List.exists2"
*)


(*
let rec mem x = function
    [] -> false
  | a::l -> a = x || mem x l

let rec memq x = function
    [] -> false
  | a::l -> a == x || memq x l

let rec assoc x = function
    [] -> raise Not_found
  | (a,b)::l -> if a = x then b else assoc x l

let rec assq x = function
    [] -> raise Not_found
  | (a,b)::l -> if a == x then b else assq x l

let rec mem_assoc x = function
  | [] -> false
  | (a, b) :: l -> a = x || mem_assoc x l

let rec mem_assq x = function
  | [] -> false
  | (a, b) :: l -> a == x || mem_assq x l

let rec remove_assoc x = function
  | [] -> []
  | (a, b as pair) :: l -> if a = x then l else pair :: remove_assoc x l

let rec remove_assq x = function
  | [] -> []
  | (a, b as pair) :: l -> if a == x then l else pair :: remove_assq x l

let rec find p = function
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l

let find_all p =
  let rec find accu = function
  | [] -> rev accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  find []

let filter = find_all

let partition p l =
  let rec part yes no = function
  | [] -> (rev yes, rev no)
  | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l in
  part [] [] l

let rec split = function
    [] -> ([], [])
  | (x,y)::l ->
      let (rx, ry) = split l in (x::rx, y::ry)

let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> (a1, a2) :: combine l1 l2
  | (_, _) -> invalid_arg "List.combine"

(** sorting *)

external obj_truncate : 'a array -> int -> unit = "obj_truncate"

let array_to_list_in_place a =
  let l = Array.length a in
  let rec loop accu n p =
    if p <= 0 then accu else begin
      if p = n then begin
        obj_truncate a p;
        loop (a.(p-1) :: accu) (n-1000) (p-1)
      end else begin
        loop (a.(p-1) :: accu) n (p-1)
      end
    end
  in
  loop [] (l-1000) l
;;

let stable_sort cmp l =
  let a = Array.of_list l in
  Array.stable_sort cmp a;
  array_to_list_in_place a
;;

let sort = stable_sort;;
*)
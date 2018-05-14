(*
   Skew Binary Random-Access List - random access to elements in O(log n)

   Copyright (c) 1999 Pierpaolo Bernardi
                      bernardp@CLI.DI.Unipi.IT

   Minor changes by Markus Mottl
                    mottl@miss.wu-wien.ac.at

   This source code is free software; you can redistribute it and/or
   modify it without any restrictions. It is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY.

   For a theoretical discussion of this data structure see
   chapter 9.3.1 in:

       Purely Functional Data Structures
       Chris Okasaki
       Cambridge University Press, 1998
       Copyright (c) 1998 Cambridge University Press
*)

exception Impossible_pattern of string

let impossible_pat x = raise (Impossible_pattern x)

type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
type 'a t    = (int * 'a tree) list

let empty = []

let cons x = function
  | ((size1,t1) :: (size2,t2) :: rest) as xs ->
      if size1 = size2 then (1+size1+size2, Node (x,t1,t2)) :: rest
      else (1, Leaf x) :: xs
  | xs -> (1, Leaf x) :: xs

let hd = function
  | [] -> failwith "hd"
  | (_, Leaf x) :: _ -> x
  | (_, Node (x,_,_)) :: _ -> x

let tl = function
  | [] -> failwith "tl"
  | (_, Leaf _) :: rest -> rest
  | (size, Node (_,t1,t2)) :: rest ->
      let size' = size lsr 1 in
      (size',t1) :: (size',t2) :: rest

let is_empty x = x = [] 

let rec tree_nth size w z =
  match (w,z) with
  | Leaf x, 0 -> x
  | Leaf _, _ -> failwith "nth"
  | Node (x,_,_), 0 -> x
  | Node (_,t1,t2), i ->
      let size' = size lsr 1 in
      if i <= size' then tree_nth size' t1 (i-1)
      else tree_nth size' t2 (i-1-size')

let rec nth x i =
  match x with
  | [] -> failwith "nth"
  | (size,t) :: rest ->
      if i < size then tree_nth size t i
      else nth rest (i-size)

let rec tree_update size w z y =
  match (w,z) with
  | Leaf _, 0 -> Leaf y
  | Leaf _, _ -> failwith "update"
  | Node (_,t1,t2), 0 -> Node (y,t1,t2)
  | Node (x,t1,t2), i ->
      let size' = size lsr 1 in
      if i <= size' then Node (x, tree_update size' t1 (i-1) y, t2)
      else Node (x,t1, tree_update size' t2 (i-1-size') y)

let rec update x i y =
  match x with
  | [] -> failwith "update"
  | (size,t) :: rest ->
      if i < size then (size, tree_update size t i y) :: rest
      else (size,t) :: update rest (i-size) y

let rec length = function
  | [] -> 0
  | (size,_) :: rest -> size + length rest

let create x n =
  (* make a list of all trees up to size n, then select *)
  (* those trees that form the greedy decomposition     *)
  let rec make size t rest =
    if size > n then rest
    else make (1+size+size) (Node (x,t,t)) ((size,t) :: rest) 

  and select m x xs =
    match (m,x) with
    | 0,_ -> xs
    | m, (size,t) :: rest -> 
        if m < size then select m rest xs
        else select (m-size) x ((size,t) :: xs)
    | _,_ -> impossible_pat "select"
  in select n (make 1 (Leaf x) []) []

let rec tree_drop size x y rest =
  match (x,y) with
  | _,0 -> (size,x) :: rest
  | Leaf _, 1 -> rest
  | Leaf _, _ -> failwith "drop"
  | Node (x,t1,t2), i ->
      let size' = size lsr 1 in
      if i <= size' then tree_drop size' t1 (i-1) ((size',t2) :: rest)
      else tree_drop size' t2 (i-1-size') rest

let rec drop x y =
  match (x,y) with
  | xs,0 -> xs
  | (size,t) :: rest, i ->
      if i < size then tree_drop size t i rest
      else drop rest (i-size)
  | _,_ -> failwith "drop"


(* Pierpaolo *)

let decompo n =
  let rec loop c top =
    let next = c+c+1 in
    if next > top then
      top-c, [c]
    else
      let rest, sizes = loop next top in
      if next = rest then
        0, next::sizes
      else if c > rest then
        rest, sizes
      else
        rest-c, c::sizes
  in
  if n < 0 then
    invalid_arg "decompo"
  else if n = 0 then
    []
  else
    let rest, sizes = loop 1 n in
    match rest with
    | 0 -> sizes
    | 1 -> 1::sizes
    | _ -> failwith "decompo"

let rec tree_rev_append t l =
  match t with
  | Leaf x -> cons x l
  | Node (x,t1,t2) -> tree_rev_append t2 (tree_rev_append t1 (cons x l))

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | (size,t1) :: t2 -> rev_append t2 (tree_rev_append t1 l2)

let rev l =
  rev_append l []

let rec tree_append t l =
  match t with
  | Leaf x -> cons x l
  | Node (x,t1,t2) -> cons x (tree_append t1 (tree_append t2 l))

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | (size,t1) :: t2 -> tree_append t1 (append t2 l2)

let rec flatten = function
  | [] -> []
  | l -> append (hd l) (flatten (tl l))

let concat = flatten

let rec tree_to_list t acc =
  match t with
  | Leaf x -> x::acc
  | Node (x,t1,t2) -> x :: tree_to_list t1 (tree_to_list t2 acc)

let to_list l =
  let rec tol l acc =
    match l with
    | [] -> acc
    | (size,t1) :: t2 -> tree_to_list t1 (tol t2 acc)
  in tol l []

let rec from_list l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | x::xs -> loop xs (cons x acc)
  in loop (List.rev l) empty

let to_array ral =
  let len = length ral in
  if len = 0 then [||]
  else
    let v = Array.make len (hd ral) in
    let rec fillist i = function
      | [] -> v
      | (size,t1) :: t2 ->
          let ni = filltree i t1 in
          fillist ni t2
    and filltree i = function
      | Leaf x -> v.(i) <- x; i+1
      | Node (x,t1,t2) ->
          v.(i) <- x;
          let ni = filltree (i+1) t1 in
          filltree ni t2
    in fillist 0 ral

let from_array a =
  let n = Array.length a in
  let rec tree_from_array da = function
    | 1 -> Leaf a.(da)
    | n ->
        let half = n lsr 1 in
        Node (a.(da),
              tree_from_array (da+1) half,
              tree_from_array (da+half+1) half)
  in
  let rec loop sc da =
    match sc with
    | [] -> []
    | x::xs -> (x, tree_from_array da x) :: loop xs (da+x)
  in loop (decompo n) 0

let rec tree_iota da = function
  | 1 -> Leaf da
  | n ->
      let half = n lsr 1 in
      Node (da, tree_iota (da+1) half, tree_iota (da+half+1) half)

let iota n =
  let rec loop sc da =
    match sc with
    | [] -> []
    | x::xs -> (x, tree_iota da x) :: loop xs (da+x)
  in loop (decompo n) 0

let rec tree_map f = function
  | Leaf x -> Leaf (f x)
  | Node (r,t1,t2) -> Node (f r, tree_map f t1, tree_map f t2)

let rec map f = function
  | [] -> []
  | (size,t1) :: t2 -> (size, tree_map f t1) :: map f t2

let rec tree_map2 f t1 t2 =
  match (t1,t2) with
  | Leaf x1, Leaf x2 -> Leaf (f x1 x2)
  | Node (r1,t11,t21), Node (r2,t12,t22) ->
      Node (f r1 r2, tree_map2 f t11 t12, tree_map2 f t21 t22)
  | _ -> impossible_pat "map2"

let rec map2 f l1 l2 =
  match (l1,l2) with
  | [],[] -> []
  | (size1,t11) :: t21, (size2,t12) :: t22 when size1 = size2 ->
      (size1, tree_map2 f t11 t12) :: map2 f t21 t22
  | _ -> invalid_arg "map2"

let rec tree_fold_left f x = function
  | Leaf i -> f x i
  | Node (r,t1,t2) -> tree_fold_left f (tree_fold_left f (f x r) t1) t2

let rec fold_left f z = function
  | [] -> z
  | (size,t1) :: t2 -> fold_left f (tree_fold_left f z t1) t2

let fold_left1 f l = fold_left f (hd l) (tl l)

let rec tree_fold_left2 f z t1 t2 =
  match (t1,t2) with
  | Leaf x1, Leaf x2 -> f z x1 x2
  | Node (r1,t11,t21), Node (r2,t12,t22) ->
      tree_fold_left2 f (tree_fold_left2 f (f z r1 r2) t11 t12) t21 t22
  | _ -> impossible_pat "fold_left"

let rec fold_left2 f z l1 l2 =
  match (l1,l2) with
  | [],[] -> z
  | (size1,t11) :: t21, (size2,t12) :: t22 when size1 = size2 ->
      fold_left2 f (tree_fold_left2 f z t11 t12) t21 t22
  | _ -> invalid_arg "fold_left2"

let rec tree_fold_right f t z =
  match t with
  | Leaf i -> f i z
  | Node (r,t1,t2) -> f r (tree_fold_right f t1 (tree_fold_right f t2 z))

let rec fold_right f l z =
  match l with
  | [] -> z
  | (size,t1) :: t2 -> tree_fold_right f t1 (fold_right f t2 z)

let rec tree_fold_right2 f t1 t2 z =
  match (t1,t2) with
  | Leaf x1, Leaf x2 -> f x1 x2 z
  | Node (r1,t11,t21), Node (r2,t12,t22) ->
      f r1 r2 (tree_fold_right2 f t11 t12 (tree_fold_right2 f t21 t22 z))
  | _ -> impossible_pat "fold_right2"

let rec fold_right2 f l1 l2 z =
  match (l1,l2) with
  | [],[] -> z
  | (size1,t11) :: t21, (size2,t12) :: t22 when size1 = size2 ->
      tree_fold_right2 f t11 t12 (fold_right2 f t21 t22 z)
  | _ -> invalid_arg "fold_right2"

let rec tree_iter f = function
  | Leaf x -> f x
  | Node (r,t1,t2) ->
      f r;
      tree_iter f t1;
      tree_iter f t2

let rec iter f = function
  | [] -> ()
  | (size,t1) :: t2 ->
      tree_iter f t1;
      iter f t2

let rec tree_iter2 f t1 t2 =
  match (t1,t2) with
  | Leaf x1, Leaf x2 -> f x1 x2
  | Node (r1,t11,t21), Node (r2,t12,t22) ->
      f r1 r2;
      tree_iter2 f t11 t12;
      tree_iter2 f t21 t22
  | _ -> impossible_pat "iter2"

let rec iter2 f l1 l2 =
  match (l1,l2) with
  | [],[] -> ()
  | (size1,t11) :: t21, (size2,t12) :: t22 when size1 = size2 ->
      tree_iter2 f t11 t12;
      iter2 f t21 t22
  | _ -> invalid_arg "iter2"

let rec tree_for_all f = function
  | Leaf x -> f x
  | Node (x,t1,t2) -> f x && tree_for_all f t1 && tree_for_all f t2

let rec for_all f = function
  | [] -> true
  | (size,t1) :: t2 -> tree_for_all f t1 && for_all f t2

let rec tree_exists f = function
  | Leaf x -> f x
  | Node (x,t1,t2) -> f x or tree_exists f t1 or tree_exists f t2

let rec exists f = function
  | [] -> false
  | (size,t1) :: t2 -> tree_exists f t1 or exists f t2

let rec tree_for_all2 f t1 t2 =
  match (t1,t2) with
  | Leaf x1, Leaf x2 -> f x1 x2
  | Node (x1,t11,t21), Node (x2,t12,t22) ->
      f x1 x2 && tree_for_all2 f t11 t12 && tree_for_all2 f t21 t22
  | _ -> impossible_pat "for_all2"

let rec for_all2 f l1 l2 =
  match (l1,l2) with
  | [],[] -> true
  | (size1,t11) :: t21, (size2,t12) :: t22 when size1 = size2 ->
      tree_for_all2 f t11 t12 && for_all2 f t21 t22
  | _ -> invalid_arg "for_all2"

let rec tree_exists2 f t1 t2 =
  match (t1,t2) with
  | Leaf x1, Leaf x2 -> f x1 x2
  | Node (x1,t11,t21), Node (x2,t12,t22) ->
      f x1 x2 or tree_exists2 f t11 t12 or tree_exists2 f t21 t22
  | _ -> impossible_pat "exists2"

let rec exists2 f l1 l2 =
  match (l1,l2) with
  | [],[] -> false
  | (size1,t11) :: t21, (size2,t12) :: t22 when size1 = size2 ->
      tree_exists2 f t11 t12 or exists2 f t21 t22
  | _ -> invalid_arg "exists2"

let rec tree_mem x = function
  | Leaf i when x = i -> true
  | Leaf i -> false
  | Node (i,_,_) when x = i -> true
  | Node (i,t1,t2) -> tree_mem x t1 or tree_mem x t2

let rec mem x = function
  | [] -> false
  | (size,t1) :: t2 -> tree_mem x t1 or mem x t2

let rec tree_memq x = function
  | Leaf i when x == i -> true
  | Leaf i -> false
  | Node (i,_,_) when x == i -> true
  | Node (i,t1,t2) -> tree_memq x t1 or tree_memq x t2

let rec memq x = function
  | [] -> false
  | (size,t1) :: t2 -> tree_mem x t1 or mem x t2

let rec tree_assoc x = function
  | Leaf (k,i) when k = x -> i
  | Leaf _ -> raise Not_found
  | Node ((k,i),_,_) when k = x -> i
  | Node ((_,_),t1,t2) ->
      try tree_assoc x t1
      with Not_found -> tree_assoc x t2

let rec assoc x = function
  | [] -> raise Not_found
  | (size,t1) :: t2 ->
      try tree_assoc x t1
      with Not_found -> assoc x t2

let rec tree_mem_assoc x = function
  | Leaf (k,_) -> k = x 
  | Node ((k,_),_,_) when k = x -> true
  | Node ((_,_),t1,t2) -> tree_mem_assoc x t1 or tree_mem_assoc x t2

let rec mem_assoc x = function
  | [] -> false
  | (size,t1) :: t2 -> tree_mem_assoc x t1 or mem_assoc x t2

let rec tree_assq x = function
  | Leaf (k,i) when k == x -> i
  | Leaf _ -> raise Not_found
  | Node ((k,i),_,_) when k == x -> i
  | Node ((_,_),t1,t2) ->
      try tree_assq x t1
      with Not_found -> tree_assq x t2

let rec assq x = function
  | [] -> raise Not_found
  | (size,t1) :: t2 ->
      try tree_assq x t1
      with Not_found -> assq x t2

let rec tree_split = function
  | Leaf (x,y) -> (Leaf x, Leaf y)
  | Node ((x,y),t1,t2) ->
      let t11, t12 = tree_split t1
      and t21, t22 = tree_split t2 in
      Node (x,t11,t21), Node (y,t12,t22)

let rec split = function
  | [] -> ([],[])
  | (size,t1) :: t2 ->
      let t11, t12 = tree_split t1 
      and t21, t22 = split t2 in
      (size,t11) :: t21, (size,t12) :: t22

let rec tree_combine t1 t2 =
  match (t1,t2) with
  | Leaf x1, Leaf x2 -> Leaf (x1,x2)
  | Node (x1,t11,t21), Node (x2,t12,t22) ->
      Node ((x1,x2), tree_combine t11 t12, tree_combine t21 t22)
  | _ -> impossible_pat "combine"

let rec combine l1 l2 =
  match (l1,l2) with
  | [],[] -> []
  | (size1,t11) :: t21, (size2,t12) :: t22 when size1 = size2 ->
      (size1, tree_combine t11 t12) :: combine t21 t22
  | _ -> invalid_arg "combine"

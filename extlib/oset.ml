(* $Id: oset.ml,v 1.2 2001/09/09 22:43:42 lefessan Exp $ *)

(* module Set = struct *)

type 'a t = Empty | Node of 'a t * 'a * 'a t * int

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2) *)

let height = function
    Empty -> 0
  | Node(_, _, _, h) -> h

    (* Creates a new node with left son l, value x and right son r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

let create l x r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, x, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

let bal l x r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Set.bal"
    | Node(ll, lv, lr, _) ->
       	if height ll >= height lr then
	  create ll lv (create lr x r)
       	else begin
	  match lr with
	    Empty -> invalid_arg "Set.bal"
	  | Node(lrl, lrv, lrr, _)->
	      create (create ll lv lrl) lrv (create lrr x r)
       	end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Set.bal"
    | Node(rl, rv, rr, _) ->
       	if height rr >= height rl then
	  create (create l x rl) rv rr
       	else begin
	  match rl with
	    Empty -> invalid_arg "Set.bal"
	  | Node(rll, rlv, rlr, _) ->
	      create (create l x rll) rlv (create rlr rv rr)
       	end
  end else
    Node(l, x, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as bal, but repeat rebalancing until the final result
       is balanced. *)

let rec join l x r =
  match bal l x r with
    Empty -> invalid_arg "Set.join"
  | Node(l', x', r', _) as t' ->
      let d = height l' - height r' in
      if d < -2 or d > 2 then join l' x' r' else t'

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assumes | height l - height r | <= 2. *)

let rec merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      bal l1 v1 (bal (merge r1 l2) v2 r2)

    (* Same as merge, but does not assume anything about l and r. *)

let rec concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      join l1 v1 (join (concat r1 l2) v2 r2)

    (* Splitting *)

let rec split ~compare x = function
    Empty ->
      (Empty, None, Empty)
  | Node(l, v, r, _) ->
      let c = compare x v in
      if c = 0 then (l, Some v, r)
      else if c < 0 then
       	let (ll, vl, rl) = split ~compare x l in (ll, vl, join rl v r)
      else
       	let (lr, vr, rr) = split ~compare x r in (join l v lr, vr, rr)

    (* Implementation of the set operations *)

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec mem ~compare x = function
    Empty -> false
  | Node(l, v, r, _) ->
      let c = compare x v in
      if c = 0 then true else
      if c < 0 then mem ~compare x l else mem ~compare x r

let rec add ~compare x = function
    Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
      let c = compare x v in
      if c = 0 then t else
      if c < 0 then bal (add ~compare x l) v r
      else bal l v (add ~compare x r)

let rec remove ~compare x = function
    Empty -> Empty
  | Node(l, v, r, _) ->
      let c = compare x v in
      if c = 0 then merge l r else
      if c < 0 then bal (remove ~compare x l) v r
      else bal l v (remove ~compare x r)

let rec union ~compare s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
      let (l2, _, r2) = split ~compare v1 t2 in
      join (union ~compare l1 l2) v1 (union ~compare r1 r2)

let rec inter ~compare s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
      match split ~compare v1 t2 with
       	(l2, None, r2) ->
	  concat (inter ~compare l1 l2) (inter ~compare r1 r2)
      | (l2, Some _, r2) ->
	  join (inter ~compare l1 l2) v1 (inter ~compare r1 r2)

let rec diff ~compare s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
      match split ~compare v1 t2 with
       	(l2, None, r2) ->
	  join (diff ~compare l1 l2) v1 (diff ~compare r1 r2)
      | (l2, Some _, r2) ->
	  concat (diff ~compare l1 l2) (diff ~compare r1 r2)

let rec compare_aux ~compare l1 l2 =
  match (l1, l2) with
    ([], []) -> 0
  | ([], _)  -> -1
  | (_, []) -> 1
  | (Empty :: t1, Empty :: t2) ->
      compare_aux ~compare t1 t2
  | (Node(Empty, v1, r1, _) :: t1, Node(Empty, v2, r2, _) :: t2) ->
      let c = compare v1 v2 in
      if c <> 0 then c else compare_aux ~compare (r1::t1) (r2::t2)
  | (Node(l1, v1, r1, _) :: t1, t2) ->
      compare_aux ~compare (l1 :: Node(Empty, v1, r1, 0) :: t1) t2
  | (t1, Node(l2, v2, r2, _) :: t2) ->
      compare_aux ~compare t1 (l2 :: Node(Empty, v2, r2, 0) :: t2)

let compare_set ~compare s1 s2 =
  compare_aux ~compare [s1] [s2]

let equal ~compare s1 s2 =
  compare_set ~compare s1 s2 = 0

let rec iter f = function
    Empty -> ()
  | Node(l, v, r, _) -> iter f l; (f v : unit); iter f r

(*
let rec fold ~f s ~acc =
  match s with
    Empty -> acc
  | Node(l, v, r, _) -> fold ~f l ~acc:(f v ~acc:(fold ~f r ~acc))
*)

let rec cardinal = function
    Empty -> 0
  | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let rec choose = function
    Empty -> raise Not_found
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> choose l

(*  end *)

class ['a] c ?(compare=Pervasives.compare) l = object (_ : 'b)
  val mutable set =
    List.fold_left (fun acc x -> add ~compare x acc) Empty l
  method contents : 'a t = set
  method set (s : 'b) = set <- s#contents
  method clear = set <- empty
  method is_empty = is_empty set
  method mem x = mem ~compare x set
  method add x = set <- add ~compare x set
  method remove x = set <- remove ~compare x set
  method union (s : 'b) = {< set = union ~compare set s#contents >}
  method inter (s : 'b) = {< set = inter ~compare set s#contents >}
  method diff (s : 'b) = {< set = diff ~compare set s#contents >}
  method compare (s : 'b) = compare_set ~compare set s#contents
  method equal (s : 'b) = equal ~compare set s#contents
  method iter ~f = iter f set
  method cardinal = cardinal set
  method elements = elements set
  method choose = choose set
end

class ['a] f ?(compare=Pervasives.compare) l = object (_ : 'b)
  val set =
    List.fold_left  (fun acc x -> add ~compare x acc) Empty l
  method contents : 'a t = set
  method is_empty = is_empty set
  method mem x = mem ~compare x set
  method add x = {< set = add ~compare x set >}
  method remove x = {< set = remove ~compare x set >}
  method union (s : 'b) = {< set = union ~compare set s#contents >}
  method inter (s : 'b) = {< set = inter ~compare set s#contents >}
  method diff (s : 'b) = {< set = diff ~compare set s#contents >}
  method compare (s : 'b) = compare_set ~compare set s#contents
  method equal (s : 'b) = equal ~compare set s#contents
  method iter ~f = iter f set
  method cardinal = cardinal set
  method elements = elements set
  method choose = choose set
end

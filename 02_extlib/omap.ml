(* $Id: omap.ml,v 1.2 2001/09/09 22:43:41 lefessan Exp $ *)

(* module Map = struct *)

type ('a,'b) t =
    Empty
  | Node of ('a,'b) t * 'a * 'b * ('a,'b) t * int

let empty = Empty

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Set.bal"
    | Node(ll, lv, ld, lr, _) ->
        if height ll >= height lr then
          create ll lv ld (create lr x d r)
        else begin
          match lr with
            Empty -> invalid_arg "Set.bal"
          | Node(lrl, lrv, lrd, lrr, _)->
              create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Set.bal"
    | Node(rl, rv, rd, rr, _) ->
        if height rr >= height rl then
          create (create l x d rl) rv rd rr
        else begin
          match rl with
            Empty -> invalid_arg "Set.bal"
          | Node(rll, rlv, rld, rlr, _) ->
              create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let rec add ~compare x data = function
    Empty ->
      Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) as t ->
      let c = compare x v in
      if c = 0 then
        Node(l, x, data, r, h)
      else if c < 0 then
        bal (add ~compare x data l) v d r
      else
        bal l v d (add ~compare x data r)

let rec find ~compare x = function
    Empty ->
      raise Not_found
  | Node(l, v, d, r, _) ->
      let c = compare x v in
      if c = 0 then d
      else find ~compare x (if c < 0 then l else r)

let rec mem ~compare x = function
    Empty ->
      false
  | Node(l, v, d, r, _) ->
      let c = compare x v in
      c = 0 || mem ~compare x (if c < 0 then l else r)

let rec merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, d1, r1, h1), Node(l2, v2, d2, r2, h2)) ->
      bal l1 v1 d1 (bal (merge r1 l2) v2 d2 r2)

let rec remove ~compare x = function
    Empty ->
      Empty
  | Node(l, v, d, r, h) as t ->
      let c = compare x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        bal (remove ~compare x l) v d r
      else
        bal l v d (remove ~compare x r)

let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
      iter f l; (f ~key:v ~data:d : unit); iter f r

(*
let rec fold ~f m ~init =
  match m with
    Empty -> init
  | Node(l, v, d, r, _) ->
      fold ~f l ~init:(f ~key:v ~data:d (fold ~f r ~init))
*)

(* end *)

class ['a,'b] c ?(compare=Pervasives.compare) l = object
  val mutable map =
    List.fold_left
      (fun acc (x,y : 'a * 'b) -> add ~compare x y acc)  Empty l
  method clear = map <- Empty
  method add ~key ~data = map <- add ~compare key data map
  method find key = find ~compare key map
  method mem key = mem ~compare key map
  method remove key = map <- remove ~compare key map
  method iter ~f = iter f map
end

class ['a,'b] f ?(compare=Pervasives.compare) l = object
  val map =
    List.fold_left
      (fun acc (x,y : 'a * 'b) -> add ~compare x y acc)  Empty l
  method add ~key ~data = {< map = add ~compare key data map >}
  method find key = find ~compare key map
  method mem key = mem ~compare key map
  method remove key = {< map = remove ~compare key map >}
  method iter ~f = iter f map
end

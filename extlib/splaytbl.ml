(* (C)        Jean-François Monin, 1999            *)
(* Centre National d'Etudes des Télécommunications *)
(* $Id: splaytbl.ml,v 1.2 2001/09/09 22:43:43 lefessan Exp $ *)
module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create : unit -> 'a t
    val clear : 'a t -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val mem : 'a t -> key -> bool
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val elements : 'a t -> (key * 'a) list
    val is_empty : 'a t -> bool
    val min_elt : 'a t -> key * 'a
    val max_elt : 'a t -> key * 'a
    val cardinal : 'a t -> int
  end

module Make(Ord: OrderedType) =
  struct
    module M = Splay.Make(Ord)
    type key = M.key
    type 'a t = 'a list M.t
    let create = M.create
    let clear = M.clear
    let find t c = List.hd (M.find t c)
    let find_all t c = (M.find t c)
    let mem = M.mem
    let add t c v =
      if M.mem t c then let r = M.find t c in M.set t c (v :: r)
      else M.add t c [v]
    let remove t c =
      if M.mem t c then let r = M.find t c in
      match r with
      |	[] -> M.remove t c
      |	[x] -> M.remove t c
      |	x::l -> M.set t c l
      else ()
    let iter f = M.iter (fun c r -> List.iter (fun v -> f c v) r)
    let id x = x
    let (++) g f = fun x -> g (f x)
    let elements t =
      let cons x y l = (x,y)::l in
      let rec app c = function
	| [] -> id
	| v::l -> cons c v ++ app c l in
      M.fold_right (fun c r -> app c r) t []
    let is_empty = M.is_empty
    let min_elt t = let c,r = M.min_elt t in c,List.hd r
    let max_elt t = let c,r = M.max_elt t in c,List.hd r
    let cardinal t = List.length (elements t)
  end

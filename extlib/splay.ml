(* (C)        Jean-François Monin, 1999            *)
(* Centre National d'Etudes des Télécommunications *)
(* ++Id: splay.ml,v 1.1 2001/04/15 18:57:13 lefessan Exp ++ *)
(* 
dcell mutable, parcours fermeture éclair
*)

let id x = x
let ( ++) g f = fun x -> g (f x)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type S =
  sig
    type key
    type 'a t
    exception Already_there
    val print : (key -> unit) -> ('a -> unit) -> 'a t -> unit
    val create: unit -> 'a t
    val clear:  'a t -> unit
    val min_elt: 'a t -> key * 'a
    val max_elt: 'a t -> key * 'a
    val find: 'a t -> key -> 'a
    val mem: 'a t -> key -> bool
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val set: 'a t -> key -> 'a -> unit
    val sub: 'a t -> key -> key -> 'a t
    val from: 'a t -> key -> 'a t
    val floor: 'a t -> key -> key * 'a
    val ceil: 'a t -> key -> key * 'a
    val prev: 'a t -> key -> key * 'a
    val next: 'a t -> key -> key * 'a
    (* functions without side effect on the tree *)
    val copy: 'a t -> 'a t
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold_left: ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val fold_right: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val cardinal: 'a t -> int
    val is_empty: 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val to_stream : 'a t -> (key * 'a) Stream.t
    val filter: (key -> 'a -> bool) -> 'a t ->  'a t
    val map: (key -> 'a -> 'b) -> 'a t ->  'b t
  end

module Make(Ord: OrderedType) =
  struct
    type key = Ord.t

    type ('a, 'b) cell = 
	{mutable lft : 'b; c : key; mutable v : 'a; mutable rgt : 'b}

    type 'a tree  = Empty | Node of ('a, 'a tree) cell

(* env = arbre croisé et avec une valeur racine bidon  *)
(* env.lft sert de continuation à droite et inversement, d'où le croisement *)
(* NOT THREAD SAFE : need a mutex *)
    let any_c : unit -> key = Obj.magic
    let any_v : unit -> 'a = Obj.magic

    type 'a t = { mutable data : 'a tree;
		  env : ('a, 'a tree) cell }

    let env_init () = {lft=Empty; c=any_c (); v=any_v (); rgt=Empty}

    let create () =
      { data = Empty; env = env_init () }

    let clear_env t = t.env.lft <- Empty; t.env.rgt <- Empty
    let clear t =
      t.data <- Empty;
      clear_env t

    let rec copy_t = function
      | Empty -> Empty
      | Node(a) -> Node({lft=copy_t a.lft; c=a.c; v=a.v; rgt=copy_t a.rgt})

    let rec depth = function
      | Empty -> 0
      | Node(a) -> max (depth a.lft) (depth a.rgt) + 1

    let print prk pr_el t = 
      let pr_elt x y = prk x; print_char ','; pr_el y in
      let ps = print_string in
      let rec pr = function
	| Empty -> ()
	| Node({lft = a; c = c; v = u; rgt = b}) -> match a,b with
	  | Empty,Empty -> pr_elt c u
	  | Empty,b -> ps "("; pr_elt c u; ps " > "; pr b; ps ")"
	  | a,Empty -> ps "(";pr a; ps " < "; pr_elt c u; ps ")"
	  | a,b -> ps "(";pr a; ps " < "; pr_elt c u; ps " > "; pr b; ps ")"
      in pr t.data; print_newline(); flush stdout
 
    (* debugging with integer keys *)
    let dbg_print s t = 
      let pr_elt x y = print_int (Obj.magic x) in
      let ps = print_string in
      let rec pr = function
	| Empty -> ()
	| Node({lft = a; c = c; v = u; rgt = b}) -> match a,b with
	  | Empty,Empty -> pr_elt c u
	  | Empty,b -> ps "("; pr_elt c u; ps " > "; pr b; ps ")"
	  | a,Empty -> ps "(";pr a; ps " < "; pr_elt c u; ps ")"
	  | a,b -> ps "(";pr a; ps " < "; pr_elt c u; ps " > "; pr b; ps ")" in
      let prt s t = Printf.printf "%s = " s; pr t in
      Printf.printf "[%s] " s; prt "data" t.data;
      prt "\nlft" t.env.rgt; prt "\nrgt" t.env.lft;
      print_newline(); flush stdout
    

(* WARNING : pred (i.e. Ord.compare_XXX) must not raise any exception *)
(* kl et kr contiennent le père de celui qui bouge *)
(* general version of traverse *)
    let gen_trav pred root fem feq =
      clear_env root;
      let rec trav t kl kr =
	match t with
	| Empty -> fem root kl kr
	| Node({lft=l; c=u; rgt=r} as ct) -> let c = pred l u r in
	  if c = 0 then
	    (kl.rgt<-l; kr.lft<-r; ct.lft<-root.env.rgt; ct.rgt<-root.env.lft;
	     root.data <- t; feq ct)
	  else if c < 0 then 
	    match l with
	    | Empty -> (kr.lft <- t; trav l kl ct)
	    | Node({lft=ll; c=v; rgt=lr} as cl) -> let c = pred ll v lr in
	      if c = 0 then (kr.lft <- t; trav l kl ct)
	      else if c < 0 then (kr.lft<-l; ct.lft<-lr; cl.rgt<-t; trav ll kl cl)
	      else (kl.rgt<-l; kr.lft<- t; trav lr cl ct)
	  else
	    match r with
	    | Empty -> (kl.rgt <- t; trav r ct kr)
	    | Node({lft=rl; c=v; rgt=rr} as cr) -> let c = pred rl v rr in
	      if c = 0 then (kl.rgt <- t; trav r ct kr)
	      else if c < 0 then (kl.rgt<-t; kr.lft <- r; trav rl ct cr)
	      else (kl.rgt<-r; ct.rgt<-rl; cr.lft<-t; trav rr cr kr)
      in trav root.data root.env root.env

    let trav_cmp x = gen_trav (fun _ u _ -> Ord.compare x u)

(* inlining of compare -> only 10 % better *)
    let trav_cmp x root fem feq =
      clear_env root;
      let rec trav t kl kr =
	match t with
	| Empty -> fem root kl kr
	| Node({lft=l; c=u; rgt=r} as ct) -> let c = Ord.compare x u in
	  if c = 0 then
	    (kl.rgt<-l; kr.lft<-r; ct.lft<-root.env.rgt; ct.rgt<-root.env.lft;
	     root.data <- t; feq ct)
	  else if c < 0 then 
	    match l with
	    | Empty -> (kr.lft <- t; trav l kl ct)
	    | Node({lft=ll; c=v; rgt=lr} as cl) -> let c = Ord.compare x v in
	      if c = 0 then (kr.lft <- t; trav l kl ct)
	      else if c < 0 then (kr.lft<-l; ct.lft<-lr; cl.rgt<-t; trav ll kl cl)
	      else (kl.rgt<-l; kr.lft<- t; trav lr cl ct)
	  else
	    match r with
	    | Empty -> (kl.rgt <- t; trav r ct kr)
	    | Node({lft=rl; c=v; rgt=rr} as cr) -> let c = Ord.compare x v in
	      if c = 0 then (kl.rgt <- t; trav r ct kr)
	      else if c < 0 then (kl.rgt<-t; kr.lft <- r; trav rl ct cr)
	      else (kl.rgt<-r; ct.rgt<-rl; cr.lft<-t; trav rr cr kr)
      in trav root.data root.env root.env

    let rescue_find root kl kr =
      if kl.rgt = Empty
      then (kr.lft <- root.env.rgt; root.data <- root.env.lft; raise Not_found)
      else (kl.rgt <- root.env.lft; root.data <- root.env.rgt; raise Not_found)
    let internal_find t x =
      trav_cmp x t rescue_find id
    let find t x =
      trav_cmp x t rescue_find (fun n -> n.v)

    let rescue_mem root kl kr =
      if kl.rgt = Empty
      then (kr.lft <- root.env.rgt; root.data <- root.env.lft; false)
      else (kl.rgt <- root.env.lft; root.data <- root.env.rgt; false)
    let resu_mem n = true
    let mem t x =
      trav_cmp x t rescue_mem resu_mem

    let go_left l u r = if l = Empty then 0 else -1
    let internal_min t cont =
      gen_trav go_left t rescue_find cont

    let go_right l u r = if r = Empty then 0 else 1
    let internal_max t cont =
      gen_trav go_right t rescue_find cont

    let min_elt t =
      let n = internal_min t id in n.c,n.v

    let max_elt t =
      let n =  internal_max t id in n.c,n.v

    let resu_add c v root kl kr =
      (kl.rgt<-Empty; kr.lft<-Empty;
       root.data <- Node({lft=root.env.rgt; c=c; v=v; rgt=root.env.lft}))

    exception Already_there
    let add t c x =
      let rescue_add n = raise Already_there in
      trav_cmp c t (resu_add c x) rescue_add

    let set t c x =
      let really_set n = n.v <- x in
      trav_cmp c t (resu_add c x) really_set

    let remove t x =
      let n = internal_find t x in
      if n.lft = Empty then t.data <- n.rgt
      else begin
	let exrgt = n.rgt in 
	t.data <- n.lft;
	let cl = internal_max t id in cl.rgt <- exrgt
      end

    let split t c cont_resc cont_ok =
      let rescue_split root kl kr =
	kl.rgt<-Empty; kr.lft<-Empty; cont_resc root in
      trav_cmp c t rescue_split cont_ok

    (* Ensures that if [floor t c = c1,_] and [ceil t c = c2,_]        *)
    (* then repeated calls to [floor t c] make [c2] at the root of     *)
    (* the right subtree of [t] (and similarly for [ceil t c and c1])  *)
    let up_neighbours t =
      if not (t.env.rgt = Empty) then
	begin
	  let exrgt = t.env.lft in
	  t.data <- t.env.rgt; internal_max t (fun x -> ());
	  t.env.rgt <- t.data; t.env.lft <- exrgt
	end;
      if not (t.env.lft = Empty) then
	begin
	  let exlft = t.env.rgt in
	  t.data <- t.env.lft; internal_min t (fun x -> ());
	  t.env.lft <- t.data; t.env.rgt <- exlft
	end
      
    let cont_floor t =
      up_neighbours t;
      let exlft = t.env.rgt and exrgt = t.env.lft in
      match exlft with
      |	Empty -> t.data <- exrgt; raise Not_found
      |	Node cl -> cl.rgt <- exrgt; t.data <- exlft; cl

    let floor t c =
      let n = split t c cont_floor id in n.c,n.v

    let cont_ceil t =
      up_neighbours t;
      let exlft = t.env.rgt and exrgt = t.env.lft in
      match exrgt with
      |	Empty -> t.data <- exlft; raise Not_found
      |	Node cl -> cl.lft <- exlft; t.data <- exrgt; cl

    let ceil t c =
      let n = split t c cont_ceil id in n.c,n.v

    let cont_prev t n = 
      let exdata = t.data in (* t.data = Node n *)
      let exlft = n.lft in 
      n.lft <- Empty;
      if exlft = Empty then raise Not_found
      else
	(t.data <- exlft;
	 let cl = internal_max t id in cl.rgt <- exdata; cl)

    let prev t c =
      let n = split t c cont_floor (cont_prev t) in n.c,n.v

    let cont_next t n = 
      let exdata = t.data in (* t.data = Node n *)
      let exrgt = n.rgt in 
      n.rgt <- Empty;
      if exrgt = Empty then raise Not_found
      else
	(t.data <- exrgt;
	 let cl = internal_min t id in cl.lft <- exdata; cl)

    let next t c =
      let n = split t c cont_ceil (cont_next t) in n.c,n.v

    let sub t c1 c2 =
      let left_c2 () =
	let src =
	  try let n2 = split t c2 cont_ceil id in n2.lft (* t.data = Node n2 *)
	  with Not_found -> t.data in
	{ data = copy_t src; env = env_init () } in
      try 
	let n1 = split t c1 cont_floor (cont_prev t) in
	let exdata1 = t.data in (* t.data = Node n1 *)
	t.data <- n1.rgt;
	let resu = left_c2 () in
	n1.rgt <- t.data; t.data <- exdata1;
	resu
      with Not_found -> left_c2 ()

    let from t c1 =
      try 
	let n1 = split t c1 cont_floor (cont_prev t) in
	{ data = copy_t n1.rgt; env = env_init () }
      with Not_found -> { data = copy_t t.data; env = env_init () }

    (* functions without side effect on the tree *)

    let copy t = { data = copy_t t.data; env = env_init () }

    let iter f t = 
      let rec iterf = function
	| Empty -> ()
	| Node(a) -> iterf a.lft; f a.c a.v; iterf a.rgt
      in iterf t.data

    let fold t f = 
      let rec foldf = function
	| Empty -> id
	| Node(a) -> foldf a.lft ++ f a.c a.v ++ foldf a.rgt
      in foldf t.data

    let fold_right f t b = 
      let rec foldr b = function
	| Empty -> b
	| Node(a) -> foldr (f a.c a.v (foldr b a.rgt)) a.lft
      in foldr b t.data

    let fold_left f b t = 
      let rec foldl b = function
	| Empty -> b
	| Node(a) ->
	    let fl = foldl b a.lft in
	    let fcv = f fl a.c a.v in foldl fcv a.rgt
      in foldl b t.data

    let cardinal t = fold_right (fun c v x -> x+1) t 0

    let is_empty t =
      let rec aux = function
	| Empty -> true
	| Node(_) -> false
      in aux t.data

    let to_list t =
      let cons x y l = (x,y)::l in fold_right cons t []

    let to_stream t =
      let cons s x y = [<s; 'x,y>] in fold_left cons [<>] t

    let filter p t =
      let rec aux x = function
	| Empty -> x
	| Node a ->
	    let fl = aux x a.lft in
	    if p a.c a.v then 
	      Node({lft = fl; c = a.c; v = a.v; rgt = aux Empty a.rgt})
	    else aux fl a.rgt in
      { data = aux Empty t.data; env = env_init () }

    let map f t =
      let rec aux = function
	| Empty -> Empty
	| Node a ->
	    Node {lft = aux a.lft; c = a.c; v = f a.c a.v; rgt = aux a.rgt} in
      { data = aux t.data; env = env_init () }

  end

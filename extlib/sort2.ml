(***********************************************************************)
(*                                                                     *)
(*                    ______________                                   *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet SOR/PARA INRIA Rocquencourt         *)
(*                                                                     *)
(*                 mail: fabrice.le_fessant@inria.fr                   *)
(*                 web:  http://www-sor.inria.fr/~lefessan             *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(*         File written in the Objective-CAML language                 *)
(***********************************************************************)

module Topo = struct
(* *)
    
    exception Cyclic
    
    type 'a node =
      { value : 'a;
        mutable indice: int;
        mutable inf_to : 'a node list; 
      }
    
    type 'a partial_order = 
      {
        mutable eles: 'a node list;
        table: ('a,'a node) Hashtbl.t;
        mutable ordered : bool;
        mutable cycle: 'a list;
      }
    
    let create () = { 
        eles = []; table = Hashtbl.create 30; ordered = false; cycle = [] }
    
    let node_add t ele =
      try
        Hashtbl.find t.table ele
      with
        Not_found ->
          let node = { value = ele; inf_to = []; indice = 0 } in
          Hashtbl.add t.table ele node;
          t.eles <- node::t.eles;
          t.ordered <- false;
          t.cycle <- [];
          node
    
    let inf t a b =
      if a <> b then
      let ta = node_add t a in
      let tb = node_add t b in
      if not(List.mem tb ta.inf_to) then
        ta.inf_to <- tb :: ta.inf_to
    
    let find_cycle node =
      let rec find visited node =
        let rec find_list visited =
          function 
            [] -> (false,false,[])
          | hd::liste ->
              let (found,look,list) = find visited hd
              in
              if found then (true,look,list) else
                find_list visited liste
        in
        if List.memq node visited then
          (true,true,[node])
        else
        let (found,look,list) = find_list (node::visited) (node.inf_to)
        in
        if found then 
          if look then
            if List.memq node list then
              (true,false,node::list)
            else
              (true,true,node::list)
          else
            (true,false,list)
        else
          (false,false,[])
      in
      find [] node
    
    let order t =
      if not t.ordered && t.cycle = [] then
        let max = (List.length t.eles)+2 in
        let rec iter n node =
          if node.indice < n then
            begin
              node.indice <- n;
              if n < max then
                List.iter (function node -> iter (n+1) node) node.inf_to
              else
                
                List.iter (fun node ->
                    let (found,look,cycle) = find_cycle node
                    in
                    if found then
                      (t.cycle <- List.map (fun node -> node.value) cycle;
                        raise Cyclic)) t.eles
            end
        in
        List.iter (function node -> node.indice <- 0) t.eles;
        List.iter (function node -> iter 1 node) t.eles;
        t.ordered <- true
    
    let less t a b =
      order t;
      let na = Hashtbl.find t.table a in
      let nb = Hashtbl.find t.table b in
      na.indice <= nb.indice
    
    let list t list =
      order t;
      Sort.list (less t) list
    
    let add t a = ignore (node_add t a)
    
    let cycle t =
      try order t; [] with Cyclic -> t.cycle
    
    let ordered t =
      try order t; true with Cyclic -> false
  
  end
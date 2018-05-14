type 'a t = {
    mutable todo_list : 'a list;
    mutable done_list : 'a list;
  }
  
let create () = { todo_list = []; done_list = []; } 
let add t v = 
  if not (List.mem v t.done_list || List.mem v t.todo_list) then
    t.todo_list <- v :: t.todo_list
    
let add_list t l = List.iter (add t) l
let add_array t a = Array.iter (add t) a
  
let rec iter t f =
  match t.todo_list with
    [] -> ()
  | v :: todo_list -> 
      t.todo_list <- todo_list;
      t.done_list <- v :: t.done_list;
      f v;
      iter t f
  
let clear t =
  t.todo_list <- [];
  t.done_list <- []
  
let restart t =
  t.todo_list <- t.done_list @ t.todo_list;
  t.done_list <- []
  
let used t v = List.mem v t.done_list
  
let apply t f v =
  add t v;
  if not (used t v) then begin
      t.todo_list <- List2.remove v t.todo_list;
      t.done_list <- v :: t.done_list;
      f v
    end
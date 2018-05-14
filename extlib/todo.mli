(*c This modules implements todo list, i.e. list of elements on which a
function has to be applied only once. *)

type 'a t  
(*d the type of a todo list *)
  
val create : unit -> 'a t
(*d [create ()] returns an empty todo list.*)
  
val add : 'a t -> 'a -> unit
(*d [add t v] adds an element to the todo list. *)
  
val add_list : 'a t -> 'a list -> unit
(*d [add_list t list] adds all the elements of [list] to the todo list. *)
  
val add_array : 'a t -> 'a array -> unit
(*d [add_array t array]  adds all the elements of [list] to the todo list. *)
  
val iter : 'a t -> ('a -> unit) -> unit  
(*d [iter t f] applies [f] on all elements of [t]. New elements can be added
to [t] by [f]. [f] will also be applied to these new elements, if it has not
been applied yet. *)
  
val clear : 'a t -> unit
(*d [clear t] removes all elements from [t]. *)
  
val restart : 'a t -> unit
(*d [restart t] is used before [iter] to forget all elements which have 
already been used. *)
  
val used : 'a t -> 'a -> bool
(*d [is_done t v] checks whether [v] has already been used. *)
  
val apply : 'a t -> ('a -> unit) -> 'a -> unit
(*d [apply t f v] adds [v] to [t] and applies [f] to [v] if it has not been 
  used yet. *)
    
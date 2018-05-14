
let rec removeq ele list =
  match list with
    e :: tail when e == ele -> removeq ele tail
  | e :: tail -> e :: (removeq ele tail)
  | _ -> []

let remove ele list =
  List.fold_left (fun list e ->
                    if e = ele then list
                    else e ::  list) [] list

  
let rec removeq_first ele list =
  match list with
    e :: tail when e == ele -> tail
  | e :: tail -> e :: (removeq_first ele tail)
  | _ -> []

let rec remove_first ele list =
  match list with
    e :: tail when e = ele -> remove_first ele tail
  | e :: tail -> e :: (remove_first ele tail)
  | _ -> []

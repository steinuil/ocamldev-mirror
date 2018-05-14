(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)


let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = String.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

(* Active buffers : can grow and reduce *)

type t = {
    mutable buffer : string;
    mutable pos_deb : int;
    mutable pos_fin : int;
    mutable size : int;
    initial_buffer : string
  }

let create n =
 let n =
   if n < 1 
   then 1 
   else if n > Sys.max_string_length 
   then Sys.max_string_length 
   else n in
 let s = String.create n in
 { buffer = s; pos_deb = 0; pos_fin = 0; size = n; initial_buffer = s }

let contents b = String.sub b.buffer b.pos_deb (b.pos_fin - b.pos_deb)

let length b = b.pos_fin - b.pos_deb

let clear b = b.pos_deb <- 0; b.pos_fin <- 0

let reset b =
  b.pos_deb <- 0; b.pos_fin <- 0; b.buffer <- b.initial_buffer;
  b.size <- String.length b.buffer

(* be carefull : the comportement is not the same than in buffer.ml *)
(* in this function, pos_deb and pos_fin can change *)
let resize b more =
  let len = b.pos_fin - b.pos_deb in
  if len + more <= b.size
  then (* no need to resize *)
    String.blit b.buffer b.pos_deb b.buffer 0 len
  else
    (let new_len = ref b.size in
    while len + more > !new_len do new_len := 2 * !new_len done;
    let new_buffer = String.create !new_len in
    String.blit b.buffer b.pos_deb new_buffer 0 len;
    b.buffer <- new_buffer;
    b.size <- !new_len);
  b.pos_deb <- 0; b.pos_fin <- len

let add_char b c =
  if b.pos_fin >= b.size then resize b 1;
  b.buffer.[b.pos_fin] <- c;
  b.pos_fin <- b.pos_fin + 1

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset + len > String.length s
  then invalid_arg "Buffer.add_substring";
  if b.pos_fin + len > b.size then resize b len;
  String.blit s offset b.buffer b.pos_fin len;
  b.pos_fin <- b.pos_fin + len

let add_string b s =
  let len = String.length s in
  if b.pos_fin + len > b.size then resize b len;
  String.blit s 0 b.buffer b.pos_fin len;
  b.pos_fin <- b.pos_fin + len
  
let add_buffer b bs =
  add_substring b bs.buffer bs.pos_deb (bs.pos_fin - bs.pos_deb)

let add_channel b ic len =
  if b.pos_fin + len > b.size then resize b len;
  really_input ic b.buffer b.pos_fin len;
  b.pos_fin <- b.pos_fin + len

let output_buffer oc b =
  output oc b.buffer b.pos_deb b.pos_fin

(* the follwing functions do not exist in the original buffer module *)
let add_subbuffer b bs offset len =
  add_substring b bs.buffer (bs.pos_deb + offset) len

let sub b offset len =
  if offset < 0 || len < 0 || offset + len > b.pos_fin - b.pos_deb
  then invalid_arg "Buffer.sub";
  b.pos_deb <- b.pos_deb + offset;
  b.pos_fin <- b.pos_deb + len

let buffer b = b.buffer, b.pos_deb

let before_read b len =
  if b.pos_fin + len > b.size then resize b len;
  b.buffer, b.pos_fin

let after_read b len =
  if b.pos_fin + len > b.size (* before read not called correctly *)
  then invalid_arg "Buffer.after_read";
  b.pos_fin <- b.pos_fin + len

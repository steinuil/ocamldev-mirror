open Unix
  
  
let list_directory filename =
  let dir = opendir filename in
  let list = ref [] in
  try
    while true do
      let file = readdir dir in 
      if file <> "." && file <> ".." then begin
          list := file :: !list 
        end;
    done;
    assert false
  with _ -> 
      closedir dir;
      !list

let is_directory filename =
  try let s = Unix.stat filename in s.st_kind = S_DIR with _ -> false

let is_link filename =
  try let s = Unix.lstat filename in s.st_kind = S_LNK with _ -> false
      

let file_size filename =
  let s = Unix.stat filename in s.st_size

let find dirname predicat action =
  
  let rec iter localdir =
    let fulldir = Filename.concat dirname localdir in
    let list = list_directory fulldir in
    let list = List.map (fun s -> Filename.concat localdir s) list in
    List.iter (fun file -> 
        if predicat file then action file) list;
    List.iter (fun file ->
        let filename = Filename.concat dirname file in
        if is_directory filename then 
          iter file) list;
  in
  iter "."

let last_extension file =
  try
    let pos = String.rindex file '.' in
    let len = String.length file in
    String.sub file (pos+1) (len - 1 -pos)
  with _ -> ""

let rec safe_mkdir dir =
  if Sys.file_exists dir then begin
      if not (is_directory dir) then 
        failwith (Printf.sprintf "%s not a directory" dir)
    end
  else begin
      let predir = Filename2.dirname dir in
      if predir <> dir then safe_mkdir predir;
      mkdir dir 0o777
    end    
      
let safe_move src_file dest_file =
  safe_mkdir (Filename2.dirname dest_file);
  Sys.rename src_file dest_file

let safe_copy src_file dest_file =
  safe_mkdir (Filename2.dirname dest_file);
  let ic = open_in_bin src_file in
  let oc = open_out_bin dest_file in
  Sys2.copy_file ic oc;
  close_in ic;
  close_out oc
    
type date_format = 
  Second
| Minute
| Hour
| Day
| WeekDay
| Month
| Year
| Space
| Colon
  
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

  
let string_of_date formats tm =
  List.fold_left (fun s format ->
      match format with
        Second -> Printf.sprintf "%s%2d" s tm.Unix.tm_sec
      | Minute -> Printf.sprintf "%s%2d" s tm.Unix.tm_min
      | Hour -> Printf.sprintf "%s%2d" s tm.Unix.tm_hour
      | Day -> Printf.sprintf "%s%2d" s tm.Unix.tm_mday
      | WeekDay  -> Printf.sprintf "%s%s" s days.(tm.Unix.tm_wday)
      | Month -> Printf.sprintf "%s%s" s months.(tm.Unix.tm_mon)
      | Year -> Printf.sprintf "%s%4d" s (1900+tm.Unix.tm_year)
      | Space -> s ^ " "
      | Colon -> s ^ ":"
  ) "" formats

external getdomainname: unit -> string = "unix_getdomainname"

let fork f x =
  let pid = Unix.fork () in
  if pid = 0 then
    (try f x ; exit 0 with e ->
          Printf.printf "Uncaught exception: %s" (Printexc.to_string e);
          print_newline (); exit 1)
  else pid

let initialized = ref false
let init () =
  if not !initialized then
    Printexc2.register_exn (fun e ->
        match e with
          Unix.Unix_error (e, f, arg) ->
            Printf.sprintf "%s failed%s: %s" f (if arg = "" then "" else 
                "on " ^ arg) (Unix.error_message e)
        | _ -> raise e
    );
  initialized := true
  
let _ = init ()
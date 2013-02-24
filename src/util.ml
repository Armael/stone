let die msg =
  print_endline msg;
  exit 1

let open_wr_flags = [Open_creat; Open_trunc; Open_text; Open_wronly]

let dump_string perm filename s =
  let chan = open_out_gen open_wr_flags perm filename in
  output_string chan s;
  close_out chan

let string_dump filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let s = String.make (len+1) '\n' in
  ignore (input chan s 0 len);
  close_in chan;
  s

let copy_file perm f1 f2 =
  let c1 = open_in f1 in
  let c2 = open_out_gen open_wr_flags perm f2 in
  (try
    while true do
      output_string c2 ((input_line c1) ^ "\n")
    done
   with End_of_file -> ());
  close_in c1;
  close_out c2

let list_of_array a =
  let l = ref [] in
  for i = 0 to Array.length a - 1 do
    l := a.(i) :: !l
  done;
  !l

(* Return the list of all the files in a given directory, with
   their relative path into the directory. *)
let explore_directory dir =
  let root = dir ^ "/" in

  let rec aux prefix = 
    let files = ref [] in
    let content = Sys.readdir (root ^ prefix) in
    Array.iter (fun it ->
      let it = prefix ^ it in
      if Sys.is_directory (root ^ it) then
        files := (aux (it ^ "/")) @ !files
      else
        files := it :: !files
    ) content;
    !files in
  aux ""

let rec remove_directory dir =
  let files = Sys.readdir dir in
  for i = 0 to Array.length files - 1 do
    let f = dir ^ "/" ^ files.(i) in
    if Sys.is_directory f then
      remove_directory f
    else
      Unix.unlink f
  done;
  Unix.rmdir dir

(* Tries to create a directory. In case of failure, do nothing *)
let try_mkdir name perm =
  try Unix.mkdir name perm with
    Unix.Unix_error _ -> ()

(* Creates all the folders needed to write in path.
   Similar to a 'mkdir -p'. *)
let mkpath path perm = 
  let i = ref 0 in
  try
    while true do
      let j = String.index_from path !i '/' in
      try_mkdir (String.sub path 0 j) perm;
      i := j+1
    done
  with Not_found -> try_mkdir path perm

(* Count the number of folders in the given path.

   Example : /foo/ -> 1
             foo/bar/baz -> 3
             foo/bar//baz -> 3
*)
let depth path =
  let d = ref 0 in
  let start = 0 in
  let end' = (String.length path - 1) in
  let succ = ref false in

  if start <= end' then (
    if path.[start] = '/' then
      decr d;
    if path.[end'] <> '/' then
      incr d;
  );

  for i = start to end' do
    if path.[i] = '/' then (
      if not !succ then (
        d := !d + 1;
        succ := true
      )
    ) else (
      succ := false
    )
  done;
  !d

(* Generate a path to depth number of folder backwards.

   Example : 3 -> ../../../
*)
let gen_backpath depth =
  let path = String.create (depth + (2 * depth)) in
  for i = 0 to depth - 1 do
    path.[3*i] <- '.';
    path.[3*i+1] <- '.';
    path.[3*i+2] <- '/'
  done;
  path
    
(* Split an absolute filename into three strings :
   - the path to the file
   - the prefix of the file (name without extension)
   - the dot (if present)
   - the extension (suffix) (if present)

   For example : 
   "/home/toto/note.txt" -> ("/home/toto", "note", ".", "txt")
   "/home/toto/bla" -> ("/home/toto", "bla", "", "")
*)
let fname_split filename =
  let path_prefix, dot, suffix =
    try let n = String.rindex filename '.' in
        (String.sub filename 0 n,
         ".",
         String.sub filename (n+1) (String.length filename - n - 1))
    with _ -> (filename, "", "") in
  let path, prefix =
    try let n = String.rindex path_prefix '/' in
        (String.sub path_prefix 0 n,
         String.sub path_prefix (n+1) (String.length path_prefix - n - 1))
    with _ -> ("", path_prefix) in
  (path, prefix, dot, suffix)

let (|>) x f = f x

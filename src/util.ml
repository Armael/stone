(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

let die msg =
  print_endline msg;
  exit 1

let open_wr_flags = [Open_creat; Open_trunc; Open_text; Open_wronly]
let open_wr_bin_flags = [Open_creat; Open_trunc; Open_binary; Open_wronly]

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

let copy_bin_file perm f1 f2 =
  let c1 = open_in_bin f1 in
  let c2 = open_out_gen open_wr_bin_flags perm f2 in
  (try
     while true do
       output_byte c2 (input_byte c1)
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

let reduce f = function
  | [] -> raise (Invalid_argument "Empty list")
  | x::xs -> List.fold_left f x xs

(* Code from ocaml batteries included *)
let interleave ?first ?last (sep:'a) (l:'a list) =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::sep::acc) t
  in
  match (l,first, last) with
  | ([], None, None) -> []
  | ([], None, Some x) -> [x]
  | ([], Some x, None) -> [x]
  | ([], Some x, Some y) -> [x;y]
  | ([h], None, None) -> [h]
  | ([h], None, Some x) -> [h;x]
  | ([h], Some x, None) -> [x;h]
  | ([h], Some x, Some y) -> [x;h;y]
  | (h::t, None , None ) -> List.rev (aux [h] t)
  | (h::t, Some x, None ) -> x::(List.rev (aux [h] t))
  | (h::t, None, Some y) -> List.rev_append (aux [h] t) [y]
  | (h::t, Some x, Some y) -> x::List.rev_append (aux [h] t) [y]

let (/^) a b = Filename.concat a b

(* Return the list of all the files in a given directory, with
   their relative path into the directory. *)
let explore_directory dir =
  let root = dir in

  let rec aux prefix = 
    let files = ref [] in
    let content = Sys.readdir (root /^ prefix) in
    Array.iter (fun it ->
      let it = prefix /^ it in
      if Sys.is_directory (root /^ it) then
        files := (aux it) @ !files
      else
        files := it :: !files
    ) content;
    !files in
  aux ""

let rec remove_directory dir =
  let files = Sys.readdir dir in
  for i = 0 to Array.length files - 1 do
    let f = dir /^ files.(i) in
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
let rec mkpath path perm =
  let continue =
    if Filename.is_relative path then
      path <> Filename.current_dir_name
    else
      path <> Sys.getcwd ()
  in
  if continue then (
    mkpath (Filename.dirname path) perm;
    try_mkdir path perm
  )

(* Count the number of folders in the given path.

   Example : /foo/ -> 1
             foo/bar/baz -> 3
             foo/bar//baz -> 3
             foo -> 1
             . -> 0
*)
let depth path =
  let depth = ref 0 in
  let path = ref path in
  while Filename.dirname !path <> !path do
    path := Filename.dirname !path;
    incr depth
  done;
  !depth

(* Generate a path to depth number of folder backwards.

   Example : 3 -> ../../../
*)
let gen_backpath depth =
  let parent_len = String.length Filename.parent_dir_name in
  let sep_len = String.length Filename.dir_sep in
  let back_len = parent_len + sep_len in
  let path = String.create (depth * back_len) in
  for i = 0 to depth - 1 do
    String.blit Filename.parent_dir_name 0 path (back_len * i) parent_len;
    String.blit Filename.dir_sep 0 path (back_len * i + parent_len) sep_len;
  done;
  path

let rec map_some f = function
  | [] -> []
  | x::xs -> match f x with
    | None -> map_some f xs
    | Some y -> y::(map_some f xs)

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let (|>) x f = f x
let (@@) f x = f x
let (%) f g = fun x -> f (g x)

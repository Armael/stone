(* Copyright (c) 2013-2025 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

let file_perm = 0o644
let dir_perm = 0o755

let open_wr_bin_flags = [Open_creat; Open_trunc; Open_binary; Open_wronly]

let read_file path =
  In_channel.with_open_bin path In_channel.input_all

let write_file ~path contents =
  Out_channel.with_open_gen open_wr_bin_flags file_perm path @@ fun cout ->
  Out_channel.output_string cout contents

let copy_bin_file f1 f2 =
  let c1 = open_in_bin f1 in
  let c2 = open_out_gen open_wr_bin_flags file_perm f2 in
  (try
     while true do
       output_byte c2 (input_byte c1)
     done
   with End_of_file -> ());
  close_in c1;
  close_out c2

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
      try
        if Sys.is_directory (root /^ it) then
          files := (aux it) @ !files
        else
          files := it :: !files
      with Sys_error _ ->
        (* Most likely, the file got removed between the call to [Sys.readdir]
           and the call to [Sys.is_directory]. *)
        ()
    ) content;
    !files in
  aux ""

let rec remove_directory dir =
  if not (Sys.file_exists dir) then ()
  else
  let files = Sys.readdir dir in
  for i = 0 to Array.length files - 1 do
    let f = dir /^ files.(i) in
    if Sys.is_directory f then
      remove_directory f
    else
      Unix.unlink f
  done;
  Unix.rmdir dir

let subdirectories dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun s -> Sys.is_directory (dir /^ s))

let mkdir name =
  Unix.mkdir name dir_perm

(* Tries to create a directory. In case of failure, do nothing *)
let try_mkdir name =
  try mkdir name with
    Unix.Unix_error _ -> ()

(* Creates all the folders needed to write in path.
   Similar to a 'mkdir -p'. *)
let rec mkpath path =
  if not (Sys.file_exists path) then (
    mkpath (Filename.dirname path);
    try_mkdir path
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

   Example:
   - 0 -> ""
   - 3 -> "../../../"
*)
let gen_backpath depth =
  String.concat "" @@ List.init depth (fun _ -> "../")

let println =
  Printf.kfprintf (fun cout -> Printf.fprintf cout "\n%!") stdout

let unwrap_result (res: ('a, string) Result.t) =
  match res with
  | Ok x -> x
  | Error msg -> println "%s" msg; exit 1

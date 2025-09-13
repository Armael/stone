(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Util

type t = Str.split_result list

let precompute (keys: string list) (s: string) =
  let regexp =
    keys
    |> interleave ~first:"\\$" "\\$\\|\\$" ~last:"\\$"
    |> reduce (^)
    |> Str.regexp
  in
  Str.full_split regexp s
  |> List.map (function
    | Str.Text x -> Str.Text x
    | Str.Delim x ->
      (* remove the $ delimiters *)
      Str.Delim (Astring.String.with_range ~first:1 ~len:(String.length x - 2) x)
  )

let fill (template: t) (data: (string * string) list) =
  let b = Buffer.create 80 in
  List.iter (function
    | Str.Text x -> Buffer.add_string b x
    | Str.Delim x -> Buffer.add_string b (List.assoc x data)
  ) template;
  Buffer.contents b

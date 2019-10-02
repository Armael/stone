(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Util

let fill_in_string (templates: (string * string) list) (s: string) =
  let keys = List.map fst templates in
  let templates = List.map (fun (s, x) ->
    ("$" ^ s ^ "$", x)) templates
  in
  let regexp =
    keys
    |> interleave ~first:"\\$" "\\$\\|\\$" ~last:"\\$"
    |> reduce (^)
    |> Str.regexp
  in
  Str.full_split regexp s
  |> List.map (function
    | Str.Text x -> x
    | Str.Delim x -> List.assoc x templates)
  |> String.concat ""

let fill template site_title page_title css bar content =
  let templates = [
    "PAGE_TITLE", page_title;
    "SITE_TITLE", site_title;
    "CSS"       , css;
    "BAR"       , bar;
    "CONTENT"   , content;
  ] in
  fill_in_string templates template

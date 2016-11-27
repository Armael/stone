(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Cow
open Util

let fill_in_string (templates: (string * Xml.t) list) (s: string) =
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
    | Str.Text x -> [`Data x]
    | Str.Delim x -> List.assoc x templates)
  |> List.flatten

let rec fill_in_xml (templates: (string * Xml.t) list) (xml: Xml.t) =
  List.map (function
    | `El (tag, frags) -> [`El (tag, fill_in_xml templates frags)]
    | `Data s -> fill_in_string templates s
  ) xml
  |> List.flatten

let fill template site_title page_title css bar content =
  let templates = [
    "PAGE_TITLE", Xml.string page_title;
    "SITE_TITLE", Xml.string site_title;
    "CSS"       , css;
    "BAR"       , bar;
    "CONTENT"   , content;
  ] in
  try
    Html.of_string template |> fill_in_xml templates
  with
    _ -> die "Error: incorrect template"

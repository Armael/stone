(* Copyright (c) 2013-2025 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Util

let template_data ~title ~page_title ~bar ~content ~path_to_root () = [
    "SITE_TITLE", title;
    "PAGE_TITLE", page_title;
    "BAR", bar;
    "CONTENT", content;
    "PATH_TO_ROOT", path_to_root;
  ]

let template_keys =
  (* hackish *)
  List.map fst (
    template_data ~title:"" ~page_title:"" ~bar:"" ~content:"" ~path_to_root:"" ()
  )

let bar ~(current: string) ~(path_to_root: string) (items: Conf.page list) =
  let open Conf in
  let open Cow in
  let mk_item {file; title} =
    let link = path_to_root ^ file in
    if current = file then (
      Xml.tag "li" ~attrs:["class", "current"] (
        Xml.tag "a" ~attrs:["href", link] (
          Xml.string title
        )
      )
    ) else (
      Xml.tag "li" (
        Xml.tag "a" ~attrs:["href", link] (
          Xml.string title
        )
      )
    )
  in
  Xml.tag "div" ~attrs:["id", "bar"] (
    Xml.tag "ul" (
      Xml.list (List.map mk_item items)
    )
  ) |> Xml.to_string

let page
    (folder: string)
    (template: Template.t)
    (conf: Conf.t)
    (source: string)
    (target: string)
    (content: string)
  =
  let page_title path =
    match List.find (fun (p: Conf.page) -> p.file = path) conf.page_title with
    | p -> p.title
    | exception Not_found ->
      Filename.remove_extension (Filename.basename target)
  in

  let path_to_root = gen_backpath (depth source - 1) in

  let bar =
    bar ~current:target ~path_to_root
      (List.map (fun file -> Conf.{ file; title = page_title file }) conf.header)
  in

  let html_page = Template.fill template (
    template_data
      ~title:conf.title
      ~page_title:(page_title target)
      ~bar
      ~content
      ~path_to_root
      ()
  )
  in

  let target_path = folder /^ "site" /^ target in
  mkpath_for target_path;
  write_file ~path:target_path html_page

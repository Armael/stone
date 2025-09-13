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

let bar ?current ~path_to_root items =
  let open Conf in
  let open Cow in
  let mk_item {file; title} =
    let link = path_to_root ^ file in
    if current = Some file then (
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
  let header_item =
    List.find_opt (fun (p: Conf.page) -> p.file = target) conf.header in

  let page_title =
    match header_item with
    | Some p -> p.title
    | None ->
      (* XXX this is a best-effort hack, but is not very satisfactory.
         If a page is not linked in the header, then there is no way to
         define its title except through the filename.
         Should there instead be some frontmatter mechanism?.. *)
      Filename.remove_extension (Filename.basename target)
  in

  let path_to_root = gen_backpath (depth source - 1) in

  let bar =
    bar ?current:(Option.map (fun (p: Conf.page) -> p.file) header_item)
      ~path_to_root conf.header
  in

  let html_page = Template.fill template (
    template_data
      ~title:conf.title
      ~page_title
      ~bar
      ~content
      ~path_to_root
      ()
  )
  in

  let target_path = folder /^ "site" /^ target in
  mkpath target_path;
  write_file ~path:target_path html_page

(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Cow
open Util
open Params

let targets conf pages =
  let exports =
    (List.map (fun (suf, exp) -> (suf, custom_exporter exp)) conf.Conf.exports)
    @ [ (".md", markdown_exporter);
        (".markdown", markdown_exporter) ] in
  List.fold_left (fun targets page ->
    try
      let (suf, exp) = List.find ((Filename.check_suffix page) % fst)
        exports in
      (page, ((Filename.chop_suffix page suf) ^ ".html", exp))::targets
    with Not_found -> targets
  ) [] pages

let bar ?current bar_pages backpath targets =
  let open Conf in
  let maybe_equal opt x = match opt with
    | None -> false
    | Some y -> x = y in
  let item {file = f; title = t} =
    let link =
      (try
        fst (List.assoc f targets)
       with Not_found -> f) in
    let link = backpath /^ link in
    if maybe_equal current f then (
      Xml.tag "li" ~attrs:["class", "current"] (
        Xml.tag "a" ~attrs:["href", link] (
          Xml.string t
        )
      )
    ) else (
      Xml.tag "li" (
        Xml.tag "a" ~attrs:["href", link] (
          Xml.string t
        )
      )
    )
  in
  Xml.tag "div" ~attrs:["id", "bar"] (
    Xml.tag "ul" (
      Xml.list (List.map item bar_pages)
    )
  ) |> Xml.to_string

let page folder template conf targets filename =
  let open Conf in
  let filepath = filename |> Filename.dirname in
  let backpath = gen_backpath (depth filepath) in

  let in_file = folder /^ pages /^ filename in
  try
    let (out_file, exporter) = List.assoc filename targets in
    let out_file = folder /^ site /^ out_file in
    let out_path = Filename.dirname out_file in
    mkpath out_path conf.dir_perm;

    let html_content = exporter in_file in
    let bar_item =
      (try let it = (List.find (fun x -> x.file = filename)
                       conf.bar_pages) in
           Some it
       with Not_found -> None) in
    let current = match bar_item with
      | Some it -> Some it.file
      | None -> None in
    let page_title = match bar_item with
      | Some it -> it.title
      | None -> Filename.remove_extension (Filename.basename filename)
    in
    let css_path = backpath  /^ static /^ css in
    let css =
      Xml.tag "link"
        ~attrs:[("href", css_path); ("type", "text/css"); ("rel", "stylesheet")]
        Xml.empty
      |> Xml.to_string
    in
    let html_page = Template.fill template
      conf.site_title page_title
      css
      (bar ?current conf.bar_pages backpath targets)
      html_content in
    dump_string conf.file_perm out_file html_page

  with Not_found ->
    let out_file = folder /^ site /^ filename in
    let out_path = Filename.dirname out_file in
    mkpath out_path conf.dir_perm;
    copy_bin_file conf.file_perm in_file out_file

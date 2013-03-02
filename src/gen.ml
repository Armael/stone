(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Cow
open Util
open Params

let custom_exporter command = fun file ->
  let command = Str.global_replace
    (Str.regexp "%{file}%")
    (Filename.quote file)
    command in
  let cin = Unix.open_process_in command in
  let output = Buffer.create 800 in
  (try
    while true do
      Buffer.add_string output (input_line cin);
      Buffer.add_char output '\n'
    done
  with End_of_file -> ());
  ignore (Unix.close_process_in cin);
  output
  |> Buffer.contents
  |> Html.of_string

let markdown_exporter = fun file ->
  string_dump file
  |> Markdown_github.of_string
  |> Markdown.to_html

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
      <:xml<
        <li class="current"><a href="$str:link$">$str:t$</a></li>
      >> 
    ) else (
      <:xml<
        <li><a href="$str:link$">$str:t$</a></li>
      >>
    )
  in
  <:xml<
    <div id="bar">
      <ul>$list:List.map item bar_pages$</ul>
    </div>
  >>

let page folder template conf targets filename =
  let open Conf in
  let prefix = filename |> Filename.basename |> Filename.chop_extension in
  let filepath = filename |> Filename.dirname in
  let backpath = gen_backpath (depth filepath) in

  let in_file = folder /^ pages /^ filename in
  try
    let (out_file, exporter) = List.assoc filename targets in
    let out_file = folder /^ site /^ out_file in
    let out_path = Filename.dirname out_file in
    mkpath out_path dir_perm;

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
      | None -> prefix in
    let css_path = backpath  /^ static /^ css in
    let css = <:xml<<link href=$str:css_path$ type="text/css" rel="stylesheet"/> >> in
    let html_page = Template.fill template
      conf.site_title page_title
      css
      (bar ?current conf.bar_pages backpath targets)
      html_content in
    let out_str = (try Html.to_string html_page with
      Parsing.Parse_error -> die ("Error : unable to generate page " ^ in_file)) in
    dump_string conf.file_perm out_file out_str

  with Not_found ->
    let out_file = folder /^ site /^ filename in
    copy_bin_file conf.file_perm in_file out_file

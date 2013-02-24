open Cow
open Util
open Params

let bar ?current bar_pages =
  let open Conf in
  let maybe_equal opt x = match opt with
    | None -> false
    | Some y -> x = y in
  let item {file = f; title = t} =
    let path, pref, _, suf = fname_split f in
    let link = path ^ pref ^ ".html" in
    if maybe_equal current t then (
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

let page folder template conf filename =
  let open Conf in
  let path, prefix, dot, suffix = fname_split filename in
  let in_file = folder ^ pages ^ filename in 
  let out_file = ref (folder ^ site ^ path ^ "/" ^ prefix) in
  mkpath (folder ^ site ^ path) dir_perm;
  let content = string_dump in_file in
  let out_str = ref "" in
  (match suffix with
  | "md" | "markdown" ->
    out_file := !out_file ^ ".html";
    let current =
      (try let page = (List.find (fun x -> x.file = filename)
                         conf.bar_pages) in
           Some page.title
       with Not_found -> None) in
    let page_title = match current with
      | Some t -> t
      | None -> prefix in
    let css_path = (gen_backpath (depth path)) ^ static ^ css in
    let css = <:xml<<link href=$str:css_path$ type="text/css" rel="stylesheet"/> >> in
    let html_content = 
      try Markdown.to_html (Markdown_github.of_string content) with
        Parsing.Parse_error -> die ("Error : unable to parse " ^ in_file) in
    let html_page = Template.fill template
      conf.site_title page_title
      css
      (bar ?current conf.bar_pages)
      html_content in
    out_str := (try Html.to_string html_page with
      Parsing.Parse_error -> die ("Error : unable do generate page " ^ in_file))

  | _ -> 
    out_file := !out_file ^ dot ^ suffix;
    out_str := content);
      
  dump_string !out_file !out_str

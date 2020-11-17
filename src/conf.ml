(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Config_file

type page = {
  file : string;
  title : string
}

type config = {
  site_title : string;
  bar_pages : page list;
  extra_static : string list;
  exports : (string * string) list;
  default_template : string;
  pages_templates : (string * string) list;
  dir_perm : int;
  file_perm : int
}

let parse_conf filename =
  let group = new group in
  let title = new string_cp ~group ["Title"] "" "Title of the website" in
  let pages = new list_cp
    (tuple2_wrappers string_wrappers string_wrappers)
    ~group
    ["Pages"] [] "List of the pages" in
  let extra_static = new list_cp string_wrappers ~group
    ["ExtraStatic"] [] "Additional files to copy from data/ to the static/ directory" in
  let exports = new list_cp
    (tuple2_wrappers string_wrappers string_wrappers)
    ~group
    ["Exports"] [] "Custom exporters (to html) for source files" in
  let default_template = new string_cp
    ~group
    ["DefaultTemplate"]
    "template.html"
    "The default html template to use" in
  let pages_templates = new list_cp
    (tuple2_wrappers string_wrappers string_wrappers)
    ~group
    ["PagesTemplates"] [] "List of couples (page, template to use)" in
  let dir_perm = new int_cp ~group ["DirPerm"] Params.dir_perm
    "Permission for the created directories" in
  let file_perm = new int_cp ~group ["FilePerm"] Params.file_perm
    "Permission for the created files" in

  group#read filename;
  { site_title = title#get;
    bar_pages = List.map (fun (f, t) -> { file = f; title = t }) pages#get;
    extra_static = extra_static#get;
    exports = exports#get;
    default_template = default_template#get;
    pages_templates = pages_templates#get;
    dir_perm = dir_perm#get;
    file_perm = file_perm#get
  }

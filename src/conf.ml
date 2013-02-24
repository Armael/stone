open Config_file

type page = {
  file : string;
  title : string
}

type config = {
  site_title : string;
  bar_pages : page list
}

let parse_conf filename =
  let group = new group in
  let title = new string_cp ~group ["Title"] "" "Title of the website" in
  let pages = new list_cp
    (tuple2_wrappers string_wrappers string_wrappers)
    ~group
    ["Pages"] [] "List of the pages" in
  
  group#read filename;
  { site_title = title#get;
    bar_pages = List.map (fun (f, t) -> { file = f; title = t }) pages#get;
  }

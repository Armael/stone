open Cow
open Util

let fill template site_title page_title css bar content = 
  let templates = [
    "PAGE_TITLE", <:xml<$str:page_title$>>;
    "SITE_TITLE", <:xml<$str:site_title$>>;
    "CSS"       , <:xml<$css$>>;
    "BAR"       , <:xml<$bar$>>;
    "CONTENT"   , <:xml<$content$>>;
  ] in
  try 
    Html.of_string ~templates template
  with 
    _ -> die "Error: incorrect template"

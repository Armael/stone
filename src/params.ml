let usage = 
"Usage : stone [OPTIONS] [FOLDER]
Manage a stone static website located in FOLDER.
If FOLDER is not specified, the current directory is used.

  -i         Setup a new static website in FOLDER
  -c         Clean FOLDER : remove the generated pages (in site/)

  no option  Build the static pages (in a directory initialized with -i)"

let data = "data/"
and pages = "pages/"
and site = "site/"
and static = "static/"

let template = "/template.html"
and css = "/style.css"
and example_index = "/index.md"
and config = "/config.stone"
and initialized_flag = "/.stone.initialized"

let dir_perm = 0o751
and file_perm = 0o644

open Cow
open Util
open Params

let init_folder folder =
  (* Check if the given folder is empty, if not, do nothing *)
  if Array.length (Sys.readdir folder) > 0 then (
    print_endline (folder ^ " is not empty. Do nothing.")

  ) else (
    (* Create folders : data contains the template and the css,
       pages contains the contents, written in Markdown or html,
       site will contain the generated pages *)
    Unix.mkdir (folder ^ data) dir_perm;
    Unix.mkdir (folder ^ pages) dir_perm;
    Unix.mkdir (folder ^ site) dir_perm;

    (* Write the template and the css *)
    dump_string (folder ^ data ^ template) Template_pak.text;
    dump_string (folder ^ data ^ css) Style_pak.text;

    (* Write the default config file *)
    dump_string (folder ^ config) Config_pak.text;

    (* Write the example home page *)
    dump_string (folder ^ pages ^ example_index) Example_index_pak.text
  )

let build_folder folder = 
  (* We assume that everything will be okay (the templates & css
     will be here) as long as the config file is present *)
  if not (Sys.file_exists (folder ^ config)) then (
    print_endline (folder ^
                     " isn't a Stone repository or isn't properly initialized");
  ) else (
    let conf = Conf.parse_conf (folder ^ config) in
    (* We will generate all pages in /pages/, even if some are not
       listed in the header bar.
       We also explore the subdirectories. *)
    let all_pages = explore_directory (folder ^ pages) in

    (* Open the template file once *)
    let template_str = string_dump (folder ^ data ^ template) in

    (* Generate all the pages in /pages/ and its subdirectories *)
    List.iter (fun page -> 
      Gen.page folder template_str conf page)
      all_pages;

    (* Copy the stylesheet into site/static/ *)
    (try Unix.mkdir (folder ^ site ^ static) dir_perm
     with Unix.Unix_error _ -> ());
    copy_file (folder ^ data ^ css) (folder ^ site ^ static ^ css)
  )

let _ =

  let init = ref false in
  let clean = ref false in
  let folders = ref [] in

  Arg.parse [
    "-i"    , Arg.Unit (fun () -> init := true), " Setup a new static website in FOLDER";
    "-c"    , Arg.Unit (fun () -> clean := true), " Clean FOLDER : remove the generated \
pages (in site/)"
  ] (fun dir -> folders := (dir ^ "/") :: !folders)
    "Usage : stone [OPTIONS] [FOLDER]...
Manage stone static websites located in the given FOLDERs.
If no FOLDER is specified, the current directory is used.
The action specified by the option is applied to all the folders\n
  no option  Build the static pages (in a directory initialized with -i)";
  
  if !folders = [] then
    folders := ["./"];
  
  List.iter (fun folder ->
    if !init then (
      if not (Sys.file_exists folder) then
        Unix.mkdir folder dir_perm;

      if not (Sys.is_directory folder) then
        print_endline (folder ^ " already exists, but is not a folder")
      else
        init_folder folder
    ) else if !clean then (
      remove_directory (folder ^ site)
    ) else (
      build_folder folder
    )) !folders

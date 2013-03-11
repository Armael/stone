(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

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
    Unix.mkdir (folder /^ data) dir_perm;
    Unix.mkdir (folder /^ pages) dir_perm;
    Unix.mkdir (folder /^ site) dir_perm;

    (* Write the templates and the css *)
    dump_string file_perm (folder /^ data /^ default_template) Template_pak.text;
    dump_string file_perm (folder /^ data /^ org_template) Org_template_pak.text;
    dump_string file_perm (folder /^ data /^ css) Style_pak.text;

    (* Write the default config file *)
    dump_string file_perm (folder /^ config) Config_pak.text;

    (* Write the example home page *)
    dump_string file_perm (folder /^ pages /^ example_index) Example_index_pak.text
  )

let build_folder folder = 
  (* We assume that everything will be okay (the templates & css
     will be here) as long as the config file is present *)
  if not (Sys.file_exists (folder /^ config)) then (
    print_endline (folder ^
                     " isn't a Stone repository or isn't properly initialized");
  ) else (
    let conf = Conf.parse_conf (folder /^ config) in
    (* We will generate all pages in /pages/, even if some are not
       listed in the header bar.
       We also explore the subdirectories. *)
    let all_pages = explore_directory (folder /^ pages) in

    (* Open the templates file once
       The result is an association list (template filename -> content) *)
    let templates_str = List.map
      (fun tpl -> (tpl, string_dump (folder /^ data /^ tpl)))
      (default_template
       :: org_template
       :: (List.map snd conf.Conf.pages_templates)) in

    (* Found target name for each page we'll have to generate.
       The result is an association list. *)
    let targets = Gen.targets conf all_pages in

    (* Generate all the pages in /pages/ and its subdirectories *)
    List.iter (fun page ->
      let template_filename =
        try List.assoc page conf.Conf.pages_templates with
          Not_found -> default_template in
      let template_str = List.assoc template_filename templates_str in

      Gen.page folder template_str conf targets page)
      all_pages;

    (* Copy the stylesheet into site/static/ *)
    (try Unix.mkdir (folder /^ site /^ static) conf.Conf.dir_perm
     with Unix.Unix_error _ -> ());
    copy_file conf.Conf.file_perm (folder /^ data /^ css)
      (folder /^ site /^ static /^ css)
  )

let _ =

  let init = ref false in
  let clean = ref false in
  let folders = ref [] in

  Arg.parse [
    "-i"    , Arg.Unit (fun () -> init := true), " Setup a new static website in FOLDER";
    "-c"    , Arg.Unit (fun () -> clean := true), " Clean FOLDER : remove the generated \
pages (in site/)"
  ] (fun dir -> folders := dir :: !folders)
    "Usage : stone [OPTIONS] [FOLDER]...
Manage stone static websites located in the given FOLDERs.
If no FOLDER is specified, the current directory is used.
The action specified by the option is applied to all the folders\n
  no option  Build the static pages (in a directory initialized with -i)";
  
  if !folders = [] then
    folders := ["."];
  
  List.iter (fun folder ->
    if !init then (
      if not (Sys.file_exists folder) then
        Unix.mkdir folder dir_perm;

      if not (Sys.is_directory folder) then
        print_endline (folder ^ " already exists, but is not a folder")
      else
        init_folder folder
    ) else if !clean then (
      remove_directory (folder /^ site)
    ) else (
      build_folder folder
    )) !folders

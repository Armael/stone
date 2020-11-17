(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Util
open Params

let get_data filename =
  match Data.read filename with
  | None -> die (Printf.sprintf "Data file not found : %s" filename)
  | Some s -> s

(* Initializes a new stone project, with default files.
*)
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
    dump_string file_perm (folder /^ data /^ default_template) (get_data "template.html");
    dump_string file_perm (folder /^ data /^ org_template) (get_data "org_template.html");
    dump_string file_perm (folder /^ data /^ css) (get_data "style.css");

    (* Write the default config file *)
    dump_string file_perm (folder /^ config) (get_data "config.stone");

    (* Write the example home page *)
    dump_string file_perm (folder /^ pages /^ example_index) (get_data "example_index.md");
  )

(* Rebuild a stone project, regenerating the contents of site/
*)
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
    let try_string_dump s =
      try Some (string_dump s)  with
        Sys_error _ -> None in
    let templates_str = map_some
        (fun tpl -> option_map
            (fun dump -> (tpl, dump))
            (try_string_dump (folder /^ data /^ tpl)))
        (default_template
         :: org_template
         :: (List.map snd conf.Conf.pages_templates)) in

    (* Found target name for each page we'll have to generate.
       The result is an association list. *)
    let targets = Gen.targets conf all_pages in

    (* Generate all the pages in /pages/ and its subdirectories *)
    List.iter (fun page ->
      let template_filename =
        try
          List.find (fun (r,_) ->
            let regexp = Str.regexp r in
            (Str.string_match regexp page 0
             && Str.matched_string page = page)
          ) conf.Conf.pages_templates
          |> snd
        with
          Not_found -> default_template in
      let template_str = List.assoc template_filename templates_str in

      Gen.page folder template_str conf targets page)
      all_pages;

    (* Copy the stylesheet and extra data files into site/static/ *)
    (try Unix.mkdir (folder /^ site /^ static) conf.Conf.dir_perm
     with Unix.Unix_error _ -> ());
    copy_bin_file conf.Conf.file_perm (folder /^ data /^ css)
      (folder /^ site /^ static /^ css);
    List.iter (fun extra_static_f ->
      try
        copy_bin_file conf.Conf.file_perm (folder /^ data /^ extra_static_f)
          (folder /^ site /^ static /^ extra_static_f)
      with Sys_error _ ->
        Printf.printf "Warning: extra data file '%s' not found\n" extra_static_f
    ) conf.Conf.extra_static;
  )

(* Watcher mode, looking for changes in data, pages or config, using inotify,
   and rebuilding when it happens.
*)
let watch_and_rebuild_stone_project inotify folder : Watch.handler =
  let rebuild_handler _ =
    Printf.printf "Rebuilding stone project \"%s\".\n%!" folder;
    build_folder folder
  in
  let rebuild_handler_rel _ = rebuild_handler in

  let data_handler =
    Watch.watch_rec_directory inotify rebuild_handler_rel (folder /^ data) in
  let pages_handler =
    Watch.watch_rec_directory inotify rebuild_handler_rel (folder /^ pages) in
  let config_handler =
    Watch.watch_file inotify rebuild_handler (folder /^ config) in

  fun event ->
    data_handler event;
    pages_handler event;
    config_handler event

let watch_stone_folders folders =
  (* First rebuild to apply any prior changes *)
  List.iter build_folder folders;
  let inotify = Inotify.create () in
  let handlers = List.map (watch_and_rebuild_stone_project inotify) folders in
  while true do
    let events = Inotify.read inotify in
    List.iter (fun event -> List.iter ((|>) event) handlers) events
  done

let () =
  let init = ref false in
  let clean = ref false in
  let watch_mode = ref false in
  let folders = ref [] in

  Arg.parse [
    "-i"    , Arg.Unit (fun () -> init := true), " Setup a new static website in FOLDER";
    "-c"    , Arg.Unit (fun () -> clean := true), " Clean FOLDER : remove the generated \
                                                   pages (in site/)";
    "-w"    , Arg.Unit (fun () -> watch_mode := true), " Watch FOLDER for modifications \
                                                        and automatically rebuild site/";
  ] (fun dir -> folders := dir :: !folders)
    "Usage : stone [OPTIONS] [FOLDER]...
Manage stone static websites located in the given FOLDERs.
If no FOLDER is specified, the current directory is used.
The action specified by the option is applied to all the folders\n
  no option  Build the static pages (in a directory initialized with -i)";

  if !folders = [] then
    folders := ["."];

  if !init then (
    List.iter (fun folder ->
      if not (Sys.file_exists folder) then
        Unix.mkdir folder dir_perm;

      if not (Sys.is_directory folder) then
        print_endline (folder ^ " already exists, but is not a folder")
      else
        init_folder folder
    ) !folders
  ) else if !clean then (
    List.iter (fun folder ->
      remove_directory (folder /^ site)
    ) !folders
  ) else if !watch_mode then (
    watch_stone_folders !folders
  ) else (
    List.iter build_folder !folders
  )

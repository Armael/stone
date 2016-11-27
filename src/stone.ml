(* Copyright (c) 2013 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Cow
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

    (* Copy the stylesheet into site/static/ *)
    (try Unix.mkdir (folder /^ site /^ static) conf.Conf.dir_perm
     with Unix.Unix_error _ -> ());
    copy_file conf.Conf.file_perm (folder /^ data /^ css)
      (folder /^ site /^ static /^ css)
  )

(* Watcher mode, looking for changes in data, pages or config, using inotify,
   and rebuilding when it happens.
*)
type rel_path = string
type handler = Inotify.event -> rel_path -> unit

let modified_selectors = [
  Inotify.S_Create;
  Inotify.S_Delete;
  Inotify.S_Modify;
  Inotify.S_Move;
]

let watch_eq w1 w2 =
  (Inotify.int_of_watch w1) = (Inotify.int_of_watch w2)

(* Watches a directory, recursively.

   Returns a handler that should be called on every inotify event; which will in
   turn call [base_handler] on each event, with the additional [rel_path]
   information, which contains the path of the sub-directory on which the event
   occured.
*)
let rec watch_rec_directory
    (inotify: Unix.file_descr)
    (base_handler: handler)
    (folder: string):
  handler
  =
  snd (watch_rec_directory_ inotify base_handler folder)

and watch_rec_directory_ inotify base_handler folder:
  Inotify.watch * handler
  =

  (* The list of the currently watched immediate subdirectories *)
  let subdirs_handlers : (string * Inotify.watch * handler) list ref =
    ref []
  in

  (* Watch a new immediate directory, and add it to the list. *)
  let watch_subdir subdir =
    let (subdir_watch, subdir_handler) =
      watch_rec_directory_
        inotify
        (fun e p -> base_handler e (subdir /^ p))
        (folder /^ subdir)
    in
    subdirs_handlers :=
      (subdir, subdir_watch, subdir_handler) ::
      !subdirs_handlers
  in
  List.iter watch_subdir (subdirectories folder);

  (* Stop watching an immediate subdirectory, and remove it from the list. *)
  let stop_watching_subdir subdir =
    subdirs_handlers :=
      List.filter (fun (subdir', subdir_watch, _) ->
          if subdir' = subdir then
            (Inotify.rm_watch inotify subdir_watch;
             false)
          else true
        ) !subdirs_handlers
  in

  (* The inotify watch for this directory ([folder]). *)
  let folder_watch = Inotify.add_watch inotify folder modified_selectors in

  folder_watch,
  fun ((w, kinds, _, path_opt) as event) rel_path ->
    if watch_eq w folder_watch then (
      begin match path_opt with
      | Some path ->
        (* If an immediate subdirectory has been created or deleted, update our
           watchs and the handler list. *)
        if List.mem Inotify.Isdir kinds then (
          if List.mem Inotify.Create kinds ||
             List.mem Inotify.Moved_to kinds
          then
            watch_subdir path
          else if List.mem Inotify.Delete kinds ||
                  List.mem Inotify.Moved_from kinds
          then
            stop_watching_subdir path
        )
      | None -> ()
      end;
      base_handler event rel_path
    ) else (
      (* If the event is not for this directory, propagate it to the handlers of
         the sub-directories. In theory, this could be optimized, but I'm not sure
         it's worth the trouble. *)
      List.iter (fun (_, _, handler) -> handler event rel_path)
        !subdirs_handlers
    )

let watch_file inotify base_handler filename : handler =
  let file_watch = Inotify.add_watch inotify filename modified_selectors in
  fun ((w, _, _, _) as event) rel_path ->
    if watch_eq file_watch w then base_handler event rel_path
    else ()

let watch_and_rebuild_stone_project inotify folder : handler =
  let rebuild_handler _ _ =
    Printf.printf "Rebuilding stone project \"%s\".\n%!" folder;
    build_folder folder
  in

  let data_handler =
    watch_rec_directory inotify rebuild_handler (folder /^ data) in
  let pages_handler =
    watch_rec_directory inotify rebuild_handler (folder /^ pages) in
  let config_handler =
    watch_file inotify rebuild_handler (folder /^ config) in

  fun event rel_path ->
    data_handler event rel_path;
    pages_handler event rel_path;
    config_handler event rel_path

let watch_stone_folders folders =
  let inotify = Inotify.create () in
  let handlers = List.map (watch_and_rebuild_stone_project inotify) folders in
  while true do
    let events = Inotify.read inotify in
    List.iter (fun event ->
      List.iter (fun handler -> handler event "") handlers
    ) events
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

(* Copyright (c) 2013-2025 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

open Util

let get_data filename =
  match Data.read filename with
  | None -> println "Data file not found: %s" filename; exit 1
  | Some s -> s

(* Initializes a new stone project, with default files.
*)
let init_folder folder =
  (* Check if the given folder is empty, if not, do nothing *)
  if Array.length (Sys.readdir folder) > 0 then (
    println "%s is not empty. Do nothing" folder;
    exit 1
  );

  (* Create folders *)
  mkdir (folder /^ "templates");
  mkdir (folder /^ "pages");
  mkdir (folder /^ "pages" /^ "static");
  mkdir (folder /^ "site");

  (* Write the template *)
  write_file ~path:(folder /^ "templates" /^ "template.html") (get_data "template.html");

  (* Write the static files *)
  Data.file_list
  |> List.filter (Astring.String.is_prefix ~affix:"static/")
  |> List.iter (fun f ->
    let target = folder /^ "pages" /^ f in
    mkpath target;
    write_file ~path:target (get_data f)
  );

  (* Write the default config file *)
  write_file ~path:(folder /^ "stone.toml") (get_data "stone.toml");

  (* Write the example home page *)
  write_file ~path:(folder /^ "pages" /^ "index.md") (get_data "example_index.md")

(* Rebuild a stone project, regenerating the contents of site/ *)
let build_folder folder =
  (* We assume that everything will be okay (the templates & css
     will be here) as long as the config file is present *)
  if not (Sys.file_exists (folder /^ "stone.toml")) then (
    println "%s isn't a Stone repository or isn't properly initialized" folder;
    exit 1
  );

  let conf = Conf.parse_conf (folder /^ "stone.toml") |> unwrap_result in

  (* We process all pages in /pages/ recursively, processing them if a rule matches,
     and copying them as-is otherwise. *)
  let all_pages = explore_directory (folder /^ "pages") in

  (* Import all files in the templates/ directory as templates.
     The result is an association list (template filename -> Template.t) *)
  let templates =
    explore_directory (folder /^ "templates")
    |> List.map (fun filename ->
      (filename, Template.precompute Gen.template_keys (read_file filename))
    )
  in

  (* Validate that all templates indicated in rules actually exist *)
  List.iter (fun (rule: Rule.t) ->
    Option.iter (fun template ->
      if Option.is_none (List.assoc_opt template templates) then (
        println "Template file '%s' not found in templates/"
          template;
        exit 1
      )
    ) rule.template
  ) conf.rules;

  (* Generate all the pages *)
  List.iter (fun file ->
    let matching_rules =
      List.filter_map (fun rule ->
        Rule.matches rule file |> Option.map (fun target -> (rule, target))
      ) conf.rules
    in
    if List.is_empty matching_rules then (
      (* Copy as-is is there is no matching rule *)
      let target = folder /^ "site" /^ file in
      mkpath target;
      copy_bin_file file target
    ) else (
      (* Apply the rules in order *)
      List.iter (fun (rule, target) ->
        mkpath target;
        let output = Rule.run rule file in
        match rule.template with
        | None -> write_file ~path:target output
        | Some template ->
          Gen.page folder (List.assoc template templates) conf file target output
      ) matching_rules;
    )
  ) all_pages

  (* Watcher mode, looking for changes in data, pages or config, using inotify,
     and rebuilding when it happens.
  *)
  let watch_and_rebuild_stone_project inotify folder : Watch.handler =
    let rebuild_handler _ =
      println "Rebuilding stone project \"%s\"." folder;
      build_folder folder
    in
    let rebuild_handler_rel _ = rebuild_handler in

    let templates_handler =
      Watch.watch_rec_directory inotify rebuild_handler_rel (folder /^ "templates") in
    let pages_handler =
      Watch.watch_rec_directory inotify rebuild_handler_rel (folder /^ "pages") in
    let config_handler =
      Watch.watch_file inotify rebuild_handler (folder /^ "stone.toml") in

    fun event ->
      templates_handler event;
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
        println "%s already exists, but is not a folder" folder
      else
        init_folder folder
    ) !folders
  ) else if !clean then (
    List.iter (fun folder ->
      remove_directory (folder /^ "site")
    ) !folders
  ) else if !watch_mode then (
    watch_stone_folders !folders
  ) else (
    List.iter build_folder !folders
  )

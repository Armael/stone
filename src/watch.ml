open Util

module Inotify = Stone_inotify

type rel_path = string
type handler = Inotify.event -> unit
type handler_with_rel_path = rel_path -> Inotify.event -> unit

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
    (base_handler: handler_with_rel_path)
    (folder: string):
  handler
  =
  snd (watch_rec_directory_ inotify base_handler folder) ""

and watch_rec_directory_ inotify base_handler folder:
  Inotify.watch * handler_with_rel_path
  =

  (* The list of the currently watched immediate subdirectories *)
  let subdirs_handlers :
    (string * Inotify.watch * handler_with_rel_path) list ref =
    ref []
  in

  (* Watch a new immediate directory, and add it to the list. *)
  let watch_subdir subdir =
    let (subdir_watch, subdir_handler) =
      watch_rec_directory_
        inotify
        (fun p e -> base_handler (subdir /^ p) e)
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
  fun rel_path ((w, kinds, _, path_opt) as event) ->
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
      base_handler rel_path event
    ) else (
      (* If the event is not for this directory, propagate it to the handlers of
         the sub-directories. In theory, this could be optimized, but I'm not sure
         it's worth the trouble. *)
      List.iter (fun (_, _, handler) -> handler rel_path event)
        !subdirs_handlers
    )

let watch_file inotify base_handler filename : handler =
  let file_watch = Inotify.add_watch inotify filename modified_selectors in
  fun ((w, _, _, _) as event) ->
    if watch_eq file_watch w then base_handler event
    else ()

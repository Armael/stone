#use "topfind";;
#require "inotify";;
#require "config-file";;
#require "cow";;
#require "str";;
#mod_use "src/util.ml";;
#mod_use "src/params.ml";;
#mod_use "src/template.ml";;
#mod_use "src/conf.ml";;
#mod_use "src/gen.ml";;
#mod_use "src/watch.ml";;

let watch_folder_dbg path =
  let inotify = Inotify.create () in
  let base_h rel e =
    Printf.printf "%s rel=%s\n%!" (Inotify.string_of_event e) rel in
  let h = Watch.watch_rec_directory inotify base_h path in
  while true do
    let events = Inotify.read inotify in
    List.iter (fun event -> h event) events
  done

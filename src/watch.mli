module Inotify := Stone_inotify

type rel_path = string
type handler = Inotify.event -> unit
type handler_with_rel_path = rel_path -> Inotify.event -> unit

val watch_rec_directory :
  Unix.file_descr ->
  handler_with_rel_path ->
  string ->
  handler

val watch_file :
  Unix.file_descr ->
  handler ->
  string ->
  handler

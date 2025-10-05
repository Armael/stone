type selector =
  | S_Access
  | S_Attrib
  | S_Close_write
  | S_Close_nowrite
  | S_Create
  | S_Delete
  | S_Delete_self
  | S_Modify
  | S_Move_self
  | S_Moved_from
  | S_Moved_to
  | S_Open
  | S_Dont_follow
  | S_Mask_add
  | S_Oneshot
  | S_Onlydir
  | S_Move
  | S_Close
  | S_All

type event_kind =
  | Access
  | Attrib
  | Close_write
  | Close_nowrite
  | Create
  | Delete
  | Delete_self
  | Modify
  | Move_self
  | Moved_from
  | Moved_to
  | Open
  | Ignored
  | Isdir
  | Q_overflow
  | Unmount

type watch
type event = watch * event_kind list * int32 * string option

val create : unit -> Unix.file_descr
val read : Unix.file_descr -> event list
val int_of_watch : watch -> int
val rm_watch : Unix.file_descr -> watch -> unit
val add_watch : Unix.file_descr -> string -> selector list -> watch

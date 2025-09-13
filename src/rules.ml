let external_command (cmd: Bos.Pat.t) (file: string): string =
  let fullcmd =
    Bos.Pat.format
      ~undef:(fun _ -> assert false (* we validated the domain of cmd at parsing time *))
      (Astring.String.Map.of_list ["source", file])
      cmd
  in
  let cin = Unix.open_process_in fullcmd in
  let output = Buffer.create 800 in
  (try
    while true do
      Buffer.add_string output (input_line cin);
      Buffer.add_char output '\n'
    done
  with End_of_file -> ());
  ignore (Unix.close_process_in cin);
  output
  |> Buffer.contents

let markdown (file: string): string =
  In_channel.with_open_bin file @@ fun cin ->
  let contents = In_channel.input_all cin in
  (* Use strict=false to get access to cmarkit extensions (why not?) *)
  let doc = Cmarkit.Doc.of_string ~strict:false contents in
  (* Use safe=false to allow including raw html. This is often convenient
     and we trust ourselves to not shoot ourselves in the foot. *)
  Cmarkit_html.of_doc ~safe:false doc

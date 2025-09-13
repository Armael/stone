module Pat = Bos.Pat

type runner =
  | External_command of {
    (* dom(cmd) included in {'source'} *)
    cmd: Pat.t;
  }
  | Builtin_markdown

type t = {
  runner: runner;
  source: Pat.t;
  (* dom(target) included in dom(source) *)
  target: Pat.t;
  template: string option;
}

(* the result of running a rule against a matching input file *)
type result = {
  target: string;
  output: string;
}

(* runners implementation *)

let external_command (cmd: Pat.t) (file: string): string =
  let fullcmd =
    Pat.format
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
  (* Use strict=false to get access to cmarkit extensions (why not?) *)
  let doc = Cmarkit.Doc.of_string ~strict:false (Util.read_file file) in
  (* Use safe=false to allow including raw html. This is often convenient
     and we trust ourselves to not shoot ourselves in the foot. *)
  Cmarkit_html.of_doc ~safe:false doc

(* *)

let matches (r: t) (filename: string): string option =
  Pat.query r.source filename
  |> Option.map (fun vars -> Pat.format vars r.target)

let run (r: t) (filename: string): string =
  match r.runner with
  | External_command { cmd } -> external_command cmd filename
  | Builtin_markdown -> markdown filename

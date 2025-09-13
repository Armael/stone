(* Copyright (c) 2013-2025 Armaël Guéneau

   See the file LICENSE for copying permission.
*)

module Pat = Bos.Pat
module AS = Astring.String
let (let*) = Result.bind
let fmt = Printf.sprintf

type page = {
  file : string;
  title : string;
}

type rule_kind =
  | External_command of {
    (* dom(cmd) = 'source' *)
    cmd: Pat.t;
  }
  | Builtin_markdown

type rule = {
  kind: rule_kind;
  (* dom(target) included in dom(source) *)
  source: Pat.t;
  target: Pat.t;
  template: string option;
}

type config = {
  title: string;
  header: page list;
  rules: rule list;
}

let parse_conf filename =
  let get_pat
      ?(check_domain: AS.Set.t -> (unit, string) Result.t = fun _ -> Ok ())
      (toml: Otoml.t)
    : Pat.t
    =
    let str = Otoml.get_string toml in
    let pat = match Pat.of_string str with
      | Ok p -> p
      | Error (`Msg msg) -> raise (Otoml.Type_error (fmt "Failed to parse pattern '%s': %s" str msg))
    in
    begin match check_domain (Pat.dom pat) with
      | Ok () -> ()
      | Error msg -> raise (Otoml.Type_error (fmt "Invalid pattern '%s': %s" str msg))
    end;
    pat
  in

  let get_rule (toml: Otoml.t): rule =
    let source = Otoml.find toml get_pat ["source"] in
    let target = Otoml.find toml (
      get_pat ~check_domain:(fun dom ->
        if AS.Set.subset dom (Pat.dom source) then Ok ()
        else Error (fmt "uses variables that do not exist in the corresponding source pattern")
      )
    ) ["target"] in
    let template = Otoml.find_opt toml Otoml.get_string ["template"] in
    let kind =
      match Otoml.find toml Otoml.get_string ["kind"] with
      | "external_command" ->
        let cmd = Otoml.find toml (
          get_pat ~check_domain:(fun dom ->
            if AS.Set.subset dom (AS.Set.of_list ["source"]) then Ok ()
            else Error (fmt "'command' can only use the $(source) variable")
          )
        ) ["command"] in
        External_command { cmd }
      | "builtin_markdown" ->
        Builtin_markdown
      | kind ->
        raise (Otoml.Type_error (fmt "Unknown rule kind: %s" kind))
    in
    { source; target; template; kind }
  in

  let* toml =
    Otoml.Parser.from_file_result filename
    |> Result.map_error (fun msg ->
      fmt "Could not parse %s as a toml file:\n%s" filename msg
    )
  in
  let* title = Otoml.find_result toml Otoml.get_string ["title"] in
  let* header = Otoml.find_result toml (Otoml.get_table_values Otoml.get_string) ["header"] in
  let* rules = Otoml.find_result toml (Otoml.get_array get_rule) ["rules"] in

  let header = List.map (fun (title, file) -> { title; file }) header in
  Ok { title; header; rules }

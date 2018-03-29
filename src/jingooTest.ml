let rec of_json: Yojson.Basic.json -> Jg_types.tvalue =
  let open Jg_types in function
    | `Assoc kv -> Tobj (List.map (fun (key, value) -> (key, of_json value)) kv)
    | `Bool b -> Tbool b
    | `Float f -> Tfloat f
    | `Int i -> Tint i
    | `List l -> Tlist (List.map of_json l)
    | `Null -> Tnull
    | `String s -> Tstr s

let to_model: Yojson.Basic.json -> (string * Jg_types.tvalue) list = function
  | `Assoc kv -> List.map (fun (key, value) -> (key, of_json value)) kv
  | `Bool b -> ["value", Jg_types.Tbool b]
  | `Float f -> ["value", Jg_types.Tfloat f]
  | `Int i -> ["value", Jg_types.Tint i]
  | `List l -> List.mapi (fun i value -> (string_of_int i, of_json value)) l
  | `Null -> ["value", Jg_types.Tnull]
  | `String s -> ["value", Jg_types.Tstr s]

let test template =
  let models = Yojson.Basic.from_channel stdin |> to_model in
  Jg_template.from_file ~models template |> print_string;
  print_newline ()

let usage_msg = "jingooTest templatefile"
let () =
  let template = ref None in
  Arg.parse []
    (fun file ->
       if !template = None then template := Some file
       else begin Arg.usage [] usage_msg; exit 1 end)
    usage_msg;
  match !template with
  | None -> Arg.usage [] usage_msg; exit 1
  | Some file -> test file

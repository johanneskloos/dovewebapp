let bufsize = 4096

let stringify_one cout varname fin =
  output_string cout "let ";
  output_string cout varname;
  output_string cout " = {|\n";
  let cin = open_in fin
  and buffer = Bytes.create bufsize
  and count = ref 0 in
  while (count := input cin buffer 0 bufsize; !count > 0) do
    output cout buffer 0 !count
  done;
  close_in cin;
  output_string cout "|}\n"

let stringify_all cout tasks =
  List.iter (fun (varname, fin) -> stringify_one cout varname fin) tasks

let to_name = function
  | '.' -> '_'
  | c -> c

let handle_file varname tasks file =
  let var = match !varname with
    | Some var -> varname := None; var
    | None -> BatString.map to_name file
  in tasks := (var, file) :: !tasks

let parse_args () =
  let usage_msg = "stringify [-o outfile] {-v varname file | file} ..."
  and varname = ref None
  and tasks = ref []
  and output = ref None in
  Arg.(parse [
      ("-o", String (fun out -> output := Some out), "output file");
      ("-v", String (fun var -> varname := Some var), "variable name")
    ]) (handle_file varname tasks) usage_msg;
  (!output, List.rev !tasks)

let () =
  let (output, tasks) = parse_args () in
  match output with
  | None -> stringify_all stdout tasks
  | Some fout ->
    let cout = open_out fout in
    stringify_all cout tasks;
    close_out cout

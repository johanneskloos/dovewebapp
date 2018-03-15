let password_encode_impl ~user ~pass =
  let cmdline = Format.sprintf "doveadm pw -u \"%s\"" user in
  let (cout, cin, cerr) = Unix.open_process_full cmdline [| |] in
  try
    for i = 1 to 2 do
      output_string cin (pass ^ "\n")
    done;
    flush cin;
    let result = try Ok (input_line cout) with End_of_file -> Error (input_line cerr)
    in match Unix.close_process_full (cout, cin, cerr) with
    | Unix.WEXITED 0 -> result
    | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
      Error (Format.sprintf "doveadm killed by signal %d" signal)
    | Unix.WEXITED status -> match result with
      | Ok _ -> Error (Format.sprintf "doveadm exited with %d" status)
      | Error e -> Error (Format.sprintf "doveadm exited with %d, %s" status e)
  with e -> ignore (Unix.close_process_full (cout, cin, cerr)); raise e

let is_bad str =
  try
    String.iter (function '\x00' | '\r' | '\n' -> raise Exit | _ -> ()) str;
    false
  with Exit -> true

let sanitize str =
  let buf = Buffer.create (String.length str) in
  String.iter (function
      | '"' -> Buffer.add_string buf "\\'"
      | c -> Buffer.add_char buf c)
    str;
  Buffer.contents buf

let password_encode ~user ~pass =
  if is_bad pass then
    Error "Password contains bad characters"
  else if is_bad user then
    Error "User name contains bad characters"
  else
    password_encode_impl ~user:(sanitize user) ~pass

let auth_impl ~user ~pass =
  let cmdline = Format.sprintf "doveadm auth test \"%s\"" user in
  let cin = Unix.open_process_out cmdline in
  try
    output_string cin (pass ^ "\n");
    flush cin;
    match Unix.close_process_out cin with
    | Unix.WEXITED 0 -> true
    | Unix.WEXITED 77 -> false
    | Unix.WEXITED s -> failwith (Format.sprintf "doveadm exited with %d" s)
    | Unix.WSIGNALED s | Unix.WSTOPPED s ->
      failwith (Format.sprintf "doveadm killed with signal %d" s)
  with e -> ignore (Unix.close_process_out cin); raise e

let auth ~user ~pass =
  if is_bad pass then
  if Str.string_match bad_pass pass 0 then
    failwith "Password contains bad characters"
  else if is_bad user then
    failwith "User name contains bad characters"
  else
    auth_impl ~user:(sanitize user) ~pass

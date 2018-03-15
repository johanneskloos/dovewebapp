let password_encode ~user ~pass =
  let cmdline = Format.sprintf "doveadm pw -u '%s'" user in
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





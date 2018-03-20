module ExtImpl = struct
  let auth = Doveadm.auth
  let password_encode = Doveadm.password_encode
  let generate_token = Token.generate
  let timestamp delay =
    Int64.of_float (Sys.time () +. float_of_int delay)
end

module M = ModelDatabase.Make(ExtImpl)
module V = ViewWeb.Make(M)(ViewCgi)
module C = Controller.Make(M)(V)(MailSendmail)

let db_handles = ref []

let take_handle () =
  match !db_handles with
  | handle :: handles -> db_handles := handles; handle
  | [] -> Database.connect Config.(get database)

let drop_handle handle =
  if List.length !db_handles > 3 then
    ignore (Sqlite3.db_close handle.Database.handle)
  else
    db_handles := handle :: !db_handles

let handle_request (cgi: Netcgi.cgi) =
  let model = take_handle ()
  and view = ViewCgi.{ cgi; set_session = None } in
  cgi # at_exit (fun () -> drop_handle model);
  match cgi # environment # cgi_path_info with
  | "/admin" -> C.event_admin model view
  | "/forgot" -> C.event_forgot model view
  | _ -> C.event_login model view
  | exception Not_found -> C.event_login model view

let run_cgi () = Netcgi_cgi.run handle_request
let run_fcgi () =
  Netcgi_fcgi.run (fun cgi -> handle_request (cgi :> Netcgi.cgi))

let usage_msg =
  "dovewebapp [-F] [-s session timeout] [-t token timeout] " ^
  "[-m maildomain]\n" ^
  "           [-D datadir] [-d database] [-n] config files...\n"

let () =
  let fcgi = ref false in
  Arg.parse
    (("-F", Arg.Set fcgi, "run under FastCGI") ::
     Config.config_args)
    Config.parse_config_file usage_msg;
  if Config.(get default_config) then
    Config.parse_config_file "/etc/dovewebapp.conf";
  if !fcgi then run_fcgi () else run_cgi ()


module ExtImpl = struct
  let auth = Doveadm.auth
  let password_encode = Doveadm.password_encode
  let generate_token = Token.generate
  let timestamp delay =
    Int64.of_float (Sys.time () +. float_of_int delay)
end

module App = Application.Make(ExtImpl)(MailSendmail)

let run_cgi () = Netcgi_cgi.run App.handle_request
let run_fcgi () =
  Netcgi_fcgi.run (fun cgi ->
      App.handle_request (cgi :> Netcgi.cgi))

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


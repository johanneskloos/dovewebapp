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

let run b () = if b then run_fcgi () else run_cgi ()

let param_run =
  let open Cmdliner.Arg in
  value & flag & info ~doc:"Run under FastCGI" ["F"; "fastcgi"]

let () = Config.run Cmdliner.Term.(const run $ param_run)

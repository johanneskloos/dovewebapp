module ExtImpl = struct
  let auth = Doveadm.auth
  let password_encode = Doveadm.password_encode
  let generate_token = Token.generate
end

module M = ModelDatabase.Make(ExtImpl)
module V = ViewWeb.Make(M)(ViewCgi)
module C = Controller.Make(M)(V)(MailSendmail)

let handle_request (cgi: Netcgi.cgi) =
  let model = Database.connect !Config.database
  and view = ViewCgi.{ cgi; set_session = None } in
  match cgi # environment # cgi_path_info with
  | "/admin" -> C.event_admin model view
  | "/forgot" -> C.event_forgot model view
  | _ -> C.event_login model view
  | exception Not_found -> C.event_login model view


let take_handle () =
  Database.connect Config.((get()).path_database)

let drop_handle handle =
  ignore (Sqlite3.db_close handle.Database.handle)

module Make(E: ModelDatabase.Externals)(Mail: Mails.Strategy) = struct
  module M = ModelDatabase.Make(E)
  module V = ViewWeb.Make(M)(ViewCgi)
  module C = Controller.Make(M)(V)(Mail)

  let handle_request ?(mailer=Mail.create ()) (cgi: Netcgi.cgi) =
    let model = take_handle ()
    and view = ViewCgi.{ cgi; set_session = None } in
    let controller = C.{ mailer; db = model } in
    cgi # at_exit (fun () -> drop_handle model);
    match cgi # environment # cgi_path_info with
    | "/admin" -> C.event_admin controller view
    | "/forgot" -> C.event_forgot controller view
    | _ -> C.event_login controller view
    | exception Not_found -> C.event_login controller view

end


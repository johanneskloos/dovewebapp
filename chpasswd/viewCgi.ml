type view = { cgi: Netcgi.cgi; mutable set_session: string option }

let get_named_argument_opt view field =
  try Some ((view.cgi # argument field) # value)
  with Not_found -> None
let enumerate_arguments view =
  List.map (fun arg -> arg # name) (view.cgi # arguments)

let get_session_data view =
  try Some (view.cgi # environment # cookie "session"
	    |> Netcgi.Cookie.value)
  with Not_found -> None

let set_session_data view token = view.set_session <- Some token
let output_page view status body =
  let cookies = match view.set_session with
    | Some token -> [Netcgi.Cookie.make "sesion" token]
    | None -> []
  and status = match status with
    | ViewWeb.StatOk -> `Ok
    | ViewWeb.StatAuth -> `Unauthorized
    | ViewWeb.StatError -> `Internal_server_error
  in
  view.set_session <- None;
  view.cgi # set_header ~set_cookies:cookies ~status ();
  view.cgi # out_channel # output_string body;
  view.cgi # out_channel # flush ();
  view.cgi # finalize ()


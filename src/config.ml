type configuration = {
  lifetime_session: int;
  lifetime_token: int;
  mail_domain: string;
  mail_host: string;
  mail_port: int;
  mail_url: string;
  path_templates: string;
  path_database: string;
}

exception NotConfigured

let current_config = ref None

let get () = match !current_config with
  | Some (cfg, _) -> cfg
  | None -> raise NotConfigured

let set_debug cfg = match !current_config with
  | None | Some (_, true) -> current_config := Some (cfg, true)
  | _ -> failwith "Setting debug configuration on production run"

let get_local_domain () =
  try
    let cin = open_in "/etc/mailname" in
    let domain = input_line cin in
    close_in cin;
    String.trim domain
  with _ -> ""

let parse_configuration fcfg zone =
  if !current_config <> None then
    failwith "Setting production configuration when already set up";
  let ini = new Inifiles.inifile fcfg in
  let cfg =
    List.fold_left (fun cfg zone ->
        List.fold_left (fun cfg attr ->
            let value = ini # getval zone attr in
            match zone with
            | "lifetime_session" ->
              { cfg with lifetime_session = int_of_string value }
            | "lifetime_token" ->
              { cfg with lifetime_token = int_of_string value }
            | "mail_domain" ->
              { cfg with mail_domain = value }
            | "mail_host" ->
              { cfg with mail_host = value }
            | "mail_port" ->
              { cfg with mail_port = int_of_string value }
            | "mail_url" ->
              { cfg with mail_url = value }
            | "path_templates" ->
              { cfg with path_templates = value }
            | "path_database" ->
              { cfg with path_database = value }
            | _ -> (* Just ignore *) cfg)
          cfg (ini # attrs zone))
      { lifetime_session = 10 * 60; (* 10 minutes *)
        lifetime_token = 3 * 3600; (* 3 hours *)
        mail_domain = get_local_domain ();
        mail_host = "localhost";
        mail_port = 25;
        path_templates = "";
        path_database = "";
        mail_url = "" }
      zone in
  current_config := Some (cfg, false)

let param_cfg =
  let open Cmdliner.Arg in
  value & opt string "/etc/dovecot/webapp.conf" & info ["c"; "config"]
    ~docv:"CFGFILE"
    ~doc:"Path to the configuration file"
let param_zone =
  let open Cmdliner.Arg in
  non_empty & pos_all string [] & info [] ~docv:"zones"
    ~doc:"Configuration zones to use"

let run parse_rest =
  let open Cmdliner.Term in
  let term =
    parse_rest
    $ (const parse_configuration
       $ param_cfg
       $ param_zone) in
  eval (term, info "dovewebapp") |>
  function
  | `Error _ -> exit 1
  | _ -> exit 0


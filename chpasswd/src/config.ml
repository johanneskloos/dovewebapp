type config_lattice = Default | ConfigFile | CommandLine
type 'a config_option = ('a * config_lattice) ref

let get ({ contents = (value, _) }: 'a config_option) = value
let set_config_file (var: 'a config_option) value =
  if snd !var <> CommandLine then
    var := (value, ConfigFile)
  else ()
let set_command_line (var: 'a config_option) value =
  var := (value, CommandLine)
let init value: 'a config_option = ref (value, Default)

let sessions_timeout = init 600 (* 10 minutes *)
let token_lifetime = init 7200 (* 2 hours *)
let domain =
  try
    let cin = open_in "/etc/mailname" in
    let domain = input_line cin in
    close_in cin;
    init domain
  with _ -> init "example.com"
let datadir = init "/usr/local/share/accountadmin"
let database = init "users.sqlite"
let default_config = init true
let mailserver = init "localhost"

let config_args =
  Arg.[
    ("-s", Int (set_command_line sessions_timeout),
     "session timeout (in seconds)");
    ("-t", Int (set_command_line token_lifetime),
     "token lifetime (in seconds)");
    ("-m", String (set_command_line domain), "Sender domain for e-mails");
    ("-M", String (set_command_line mailserver), "Mail server for SMTP");
    ("-D", String (set_command_line datadir), "Data directory");
    ("-d", String (set_command_line database), "Path to sqlite user database");
    ("-n", Unit (fun () -> set_command_line default_config false),
     "Do not read default config file")
  ]

let parse_config_file filename =
  let lineno = ref 0
  and cin = open_in filename in
  try
    while true do
      let line = input_line cin in
      incr lineno;
      let line = try
	  let index = String.index line '#' in String.sub line 0 index
	with Not_found -> line in
      if line <> "" then
	try
	  let index = String.index line '=' in
	  let rest = String.sub line (index + 1) (-1) in
	  match String.sub line 0 index with
	  | "session_timeout" ->
	    set_config_file sessions_timeout (int_of_string rest)
	  | "token_lifetime" ->
	    set_config_file token_lifetime (int_of_string rest)
	  | "domain" -> set_config_file domain rest
	  | "data_directory" -> set_config_file datadir rest
	  | "database" -> set_config_file database rest
	  | "mailserver" -> set_config_file mailserver rest
	  | key ->
	    Format.eprintf "%s(%d): Unknown key %s" filename !lineno key;
	    exit 1
	with
	| Not_found ->
	  Format.eprintf "%s(%d): Syntax error, no '='" filename !lineno;
	  exit 1
    done
  with
  | End_of_file -> ()

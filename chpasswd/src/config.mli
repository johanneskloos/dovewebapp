type 'a config_option
val get : 'a config_option -> 'a
val set_config_file : 'a config_option -> 'a -> unit
val set_command_line : 'a config_option -> 'a -> unit
val sessions_timeout : int config_option
val token_lifetime : int config_option
val domain : string config_option
val datadir : string config_option
val database : string config_option
val mailserver : string config_option
val default_config : bool config_option
val config_args : (string * Arg.spec * string) list
val parse_config_file : string -> unit

(** Configuration parameters for dovewebapp. *)

type 'a config_option
(** Configuration options. The [config_option] type
    implements a container tracking the provenance of a value:
    Default, from configuration file, from command line. *)
val get : 'a config_option -> 'a
val set_config_file : 'a config_option -> 'a -> unit
(** Set a configuration option with configuration file provenance.
    This will overwrite existing values with default, configuration file
    and command line provenance. *)
val set_command_line : 'a config_option -> 'a -> unit
(** Set a configuration option with command line provenance.
    This will overwrite existing values with default and command line
    provenance, but not with configuration file provenacen. *)
val sessions_timeout : int config_option
(** The lifetime of an admin session, in seconds. *)
val token_lifetime : int config_option
(** The lifetime of a forgot password token, in seconds. *)
val domain : string config_option
(** The e-mail domain of the user names in the dovecot user database. *)
val datadir : string config_option
(** The directory containing program data. *)
val database : string config_option
(** The path to the dovecot user database. *)
val mailserver : string config_option
(** The host name of the mail server to send notification e-mails. *)
val mailport : int config_option
(** The TCP port of the mail server to send notification e-mails. *)
val default_config : bool config_option
(** Whether to load the default config file. *)
val config_args : (string * Arg.spec * string) list
(** Configuratin option specification, for Arg.parse *)
val parse_config_file : string -> unit
(** Parse a config file. *)

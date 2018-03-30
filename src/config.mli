(** Configuration parameters for dovewebapp. *)

type configuration = {
  lifetime_session: int;
  (** The lifetime of an admin session, in seconds. *)
  lifetime_token: int;
  (** The lifetime of a forgot password token, in seconds. *)
  mail_domain: string;
  (** The e-mail domain of the user names in the dovecot user database. *)
  mail_host: string;
  (** The host name of the mail server to send notification e-mails. *)
  mail_port: int;
  (** The TCP port of the mail server to send notification e-mails. *)
  mail_url: string;
  (** The base URL for the web interface. *)
  path_templates: string;
  (** The directory containing program data. *)
  path_database: string;
  (** The path to the dovecot user database. *)
}

exception NotConfigured

val get: unit -> configuration
(** Get the currently-enabled configuration.

    @raises NotConfigured The current configuration has not yet been set. *)

val set_debug: configuration -> unit
(** Set the currently-enabled configuration to a debug configuration. *)

val run: (unit -> unit) Cmdliner.Term.t -> unit
(** Run the given continuation with the configuration derived from the
    command line. *)

(** E-Mail sending tool. *)

module type Strategy = sig
  (** For debugging purposes, we allow mocking the actual
      message emission. *)

  type t
  (** Data used for connection to a mailer. *)

  val create: unit -> t
  (** Create a mailer *)

  val send_mail : t -> string -> Netmime.complex_mime_message -> unit
  (** [send_mail mailer address message] sends an e-mail to envelope address
      [address] with message [message]. *)
end
module Make(Strat : Strategy):
sig
  val send_token_email :
    Strat.t -> email:Model.email -> user:string -> token:string -> unit
  (** Send an e-mail to [user] containing a link to the forgotten password
      page, using token [token]. The user's e-mail address can be
      given using the [email] parameter, passing [Address _];
      if [email] is [NoAddress], the local address of the user
      is inferred from [user] and [Config.domain]. *)

  val send_account_email : Strat.t -> email:string -> token:string -> unit
  (** Send an e-mail to [email] containing a link to the forgotten password
      page, using token [token]. *)

  val create: unit -> Strat.t
  (** Create a mailer *)
end

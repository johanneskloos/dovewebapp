(** E-Mail sending tool. *)

module type Strategy = sig
  (** For debugging purposes, we allow mocking the actual
      message emission. *)

  val send_mail : string -> Netmime.complex_mime_message -> unit
  (** [send_mail address message] sends an e-mail to envelope address
      [address] with message [message]. *)
end
module Make(Strat : Strategy):
sig
  val send_token_email :
    email:Model.email -> user:string -> token:string -> unit
  (** Send an e-mail to [user] containing a link to the forgotten password
      page, using token [token]. The user's e-mail address can be
      given using the [email] parameter, passing [Address _];
      if [email] is [NoAddress], the local address of the user
      is inferred from [user] and [Config.domain]. *)

  val send_account_email : email:string -> token:string -> unit
  (** Send an e-mail to [email] containing a link to the forgotten password
      page, using token [token]. *)
end

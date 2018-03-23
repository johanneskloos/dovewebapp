module type Strategy = sig
  val send_mail : string -> Netmime.complex_mime_message -> unit
end
module Make(Strat : Strategy):
sig
  val send_token_email :
    email:Model.email -> user:string -> token:string -> unit
  val send_account_email : email:string -> token:string -> unit
end

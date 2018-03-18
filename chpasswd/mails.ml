module type Strategy = sig
  val send_mail: string -> Netmime.complex_mime_message -> unit
end

module Make(Strat: Strategy) = struct
  let format_and_send_mail subject template ~email ~token =
    let models = [
      ("token", Jg_types.Tstr token)
    ] in
    let body = Netsendmail.compose
      ~from_addr:("Account management",
		  "no-reply@" ^ !Config.domain)
      ~to_addrs:[("", email)]
      ~subject
      (Template.from_file ~models template) in
    Strat.send_mail email body

  let send_token_email ~email ~user ~token =
    let email = match email with
      | Some address -> address
      | None -> user ^ "@" ^ !Config.domain
    in format_and_send_mail "Forgotten password" "forgot.822" ~email ~token

  let send_account_email ~email ~token =
    format_and_send_mail "Your new account" "new.822" ~email ~token
end


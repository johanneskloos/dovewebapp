let send_mail subject template ~email ~token =
  let models = [
    ("mail", Jg_types.Tstr email);
    ("token", Jg_types.Tstr token)
  ] in let message = Netsendmail.compose
      ~from_addr:("Account management",
		  "no-reply@" ^ !Config.domain)
      ~to_addrs:[("", email)]
      ~subject
      (Template.from_file ~models template)
  in ignore message

let send_token_email ~email ~user ~token =
  let email = match email with
    | Some address -> address
    | None -> user ^ "@" ^ !Config.domain
  in send_mail "Forgotten password" "forgot.822" ~email ~token

let send_account_email = send_mail "Your new account" "new.822"

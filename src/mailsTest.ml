open OUnit2

module M = Mails.Make(MailMock)

let id x = x

let check_mail address subject sort url (address_msg, message) =
  let buf = Buffer.create 4096 in
  let channel = new Netchannels.output_buffer buf in
  Netmime_channels.write_mime_message ~wr_header:false channel message;
  assert_equal ~printer:id ~msg:"Message bodies"
    url
    (Buffer.contents buf |> String.trim);
  assert_equal ~printer:Fmt.(to_to_string (list string)) ~msg:"Addresses"
    [" <" ^ address ^ ">"] ((fst message) # multiple_field "to");
  assert_equal ~printer:id ~msg:"Subjects" subject
    ((fst message) # field "subject");
  assert_equal ~printer:id ~msg:"Envelope address"
    address address_msg

let check_mails mailer address subject sort url =
  match !mailer with
  | [(mail)] -> check_mail address subject sort url mail
  | _ -> assert_failure "Expected exactly one e-mail"

let test_token_nomail =
  "Test mail construction: forgot, no alt email" >:: fun ctx ->
    TestTools.set_up ctx;
    let mailer = MailMock.create () in
    M.send_token_email mailer ~email:NoAddress ~user:"foo" ~token:"xyz";
    check_mails mailer "foo@example.com" "Forgotten password"
      "forgot" "xyz"

let test_token_mail =
  "Test mail construction: forgot, alt email" >:: fun ctx ->
    TestTools.set_up ctx;
    let mailer = MailMock.create () in
    M.send_token_email mailer ~email:(Address "user@example.net")
      ~user:"foo" ~token:"xyz";
    check_mails mailer "user@example.net" "Forgotten password"
      "forgot" "xyz"

let test_token_nouser =
  "Test mail construction: no such user" >:: fun ctx ->
    TestTools.set_up ctx;
    let mailer = MailMock.create () in
    M.send_token_email mailer ~email:NoSuchUser
      ~user:"foo" ~token:"xyz";
    assert_equal 0 (List.length !mailer)

let test_new_mail =
  "Test mail construction: new" >:: fun ctx ->
    TestTools.set_up ctx;
    let mailer = MailMock.create () in
    M.send_account_email mailer ~email:"user@example.net"
      ~token:"xyz" ~user:"user";
    check_mails mailer "user@example.net" "Your new account"
      "new" "xyz"

let tests =
  "Mails" >::: [test_token_nomail; test_token_mail; test_new_mail]

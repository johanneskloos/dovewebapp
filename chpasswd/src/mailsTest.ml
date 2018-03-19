open OUnit2

module M = Mails.Make(MailMock)

let id x = x

let check_mail address subject sort url (address_msg, message) =
  let buf = Buffer.create 4096 in
  let channel = new Netchannels.output_buffer buf in
  Netmime_channels.write_mime_message ~wr_header:false channel message;
  assert_equal ~printer:id ~msg:"Message bodies"
    ("HEAD " ^ sort ^ "\r\n" ^ url ^ "\r\nTAIL\r\n")
    (Buffer.contents buf);
  assert_equal ~printer:Fmt.(to_to_string (list string)) ~msg:"Addresses"
    [" <" ^ address ^ ">"] ((fst message) # multiple_field "to");
  assert_equal ~printer:id ~msg:"Subjects" subject
    ((fst message) # field "subject");
  assert_equal ~printer:id ~msg:"Envelope address"
    address address_msg

let check_mails address subject sort url =
  match !MailMock.mails with
  | [(mail)] -> check_mail address subject sort url mail
  | _ -> assert_failure "Expected exactly one e-mail"

let write_template sort dir =
  let chan = open_out (Filename.concat dir (sort ^ ".822")) in
  Format.fprintf (Format.formatter_of_out_channel chan)
    "HEAD %s\nsetpw://{{ token }}\nTAIL@." sort;
  close_out chan

let setup_test ctx =
  let dir = bracket_tmpdir ctx in
  write_template "forgot" dir;
  write_template "new" dir;
  Config.(set_command_line datadir dir);
  Config.(set_command_line domain "example.com");
  MailMock.mails := []

let make_mail_test ~title fn =
  title >:: fun ctx ->
    setup_test ctx; fn ()

let test_token_nomail =
  "Test mail construction: forgot, no alt email" >:: fun ctx ->
    setup_test ctx;
    M.send_token_email ~email:None ~user:"foo" ~token:"xyz";
    check_mails "foo@example.com" "Forgotten password"
      "forgot" "setpw://xyz"

let test_token_mail =
  "Test mail construction: forgot, no alt email" >:: fun ctx ->
    setup_test ctx;
    M.send_token_email ~email:(Some "user@example.net")
      ~user:"foo" ~token:"xyz";
    check_mails "user@example.net" "Forgotten password"
      "forgot" "setpw://xyz"

let test_new_mail =
  "Test mail construction: new" >:: fun ctx ->
    setup_test ctx;
    M.send_account_email ~email:"user@example.net"
      ~token:"xyz";
    check_mails "user@example.net" "Your new account"
      "new" "setpw://xyz"

let tests = "Mails" >:::  [test_token_nomail; test_token_mail; test_new_mail]

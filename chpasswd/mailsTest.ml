open Kaputt

module M = Mails.Make(MailMock)

let check_mail address subject sort url (address_msg, message) =
  let buf = Buffer.create 4096 in
  let channel = new Netchannels.output_buffer buf in
  Netmime_channels.write_mime_message ~wr_header:false channel message;
  Assertion.equal_string ~msg:"Message bodies"
    ("HEAD " ^ sort ^ "\r\n" ^ url ^ "\r\nTAIL\r\n")
    (Buffer.contents buf);
  Assertion.make_equal_list (=) (fun x -> x) ~msg:"Addresses"
    [" <" ^ address ^ ">"] ((fst message) # multiple_field "to");
  Assertion.equal_string ~msg:"Subjects" subject
    ((fst message) # field "subject");
  Assertion.equal_string ~msg:"Evnelope address"
    address address_msg

let write_template sort dir =
  let chan = open_out (Filename.concat dir (sort ^ ".822")) in
  Format.fprintf (Format.formatter_of_out_channel chan)
    "HEAD %s\nsetpw://{{ token }}\nTAIL@." sort;
  close_out chan

let setup_test () =
  let dir = DatabaseTestTools.setup_tmpdir () in
  write_template "forgot" dir;
  write_template "new" dir;
  Config.(set_command_line datadir dir);
  Config.(set_command_line domain "example.com");
  MailMock.mails := [];
  dir

let make_mail_test ~title fn =
  Test.make_assert_test ~title setup_test (fun dir -> fn (); dir)
    DatabaseTestTools.delete_tmpdir

let test_token_nomail =
  make_mail_test ~title:"Test mail construction: forgot, no alt email"
    (fun () ->
       M.send_token_email ~email:None ~user:"foo" ~token:"xyz";
       match !MailMock.mails with
       | [(mail)] ->
	 check_mail "foo@example.com" "Forgotten password"
	   "forgot" "setpw://xyz" mail
       | _ -> Assertion.fail_msg "Expected one E-Mail")

let test_token_mail =
  make_mail_test ~title:"Test mail construction: forgot, no alt email"
    (fun () ->
       M.send_token_email ~email:(Some "user@example.net")
	 ~user:"foo" ~token:"xyz";
       match !MailMock.mails with
       | [(mail)] ->
	 check_mail "user@example.net" "Forgotten password"
	   "forgot" "setpw://xyz" mail
       | _ -> Assertion.fail_msg "Expected one E-Mail")

let test_new_mail =
  make_mail_test ~title:"Test mail construction: new"
    (fun () ->
       M.send_account_email ~email:"user@example.net"
	 ~token:"xyz";
       match !MailMock.mails with
       | [(mail)] ->
	 check_mail "user@example.net" "Your new account"
	   "new" "setpw://xyz" mail
       | _ -> Assertion.fail_msg "Expected one E-Mail")

let () =
  Test.run_tests [test_token_nomail; test_token_mail; test_new_mail]

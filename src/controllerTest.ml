open OUnit2
open Controller
open TestTools

let validate_user_1 =
  "Validate valid user name" >:: fun ctx ->
    let user = "Harry.Potter" in
    assert_equal ~printer:Fmt.(to_to_string string)
      user (validate_user user)

let validate_user_2 =
  "Validate invalid user name" >:: fun ctx ->
    let user = "Harry\nPotter" in
    assert_raises (InvalidUser user)
      (fun () -> validate_user user)

let tests_validate_user = [validate_user_1; validate_user_2]

let validate_pass_1 =
  "Validate valid password" >:: fun ctx ->
    let pass = "Expecto Patronum!" in
    assert_equal ~printer:Fmt.(to_to_string string)
      pass (validate_pass pass)

let validate_pass_2 =
  "Validate invalid password" >:: fun ctx ->
    assert_raises (InvalidPass)
      (fun () -> validate_pass "Expecto\nPatronum!")

let tests_validate_pass = [validate_pass_1; validate_pass_2]

let validate_email_1 =
  "Validate valid e-mail address" >:: fun ctx ->
    let email = "harry.potter@hogwarts.ac.uk" in
    assert_equal ~printer:Fmt.(to_to_string string)
      email (validate_email email)
let validate_email_2 =
  "Validate invalid e-mail address" >:: fun ctx ->
    let email = "nope! - this is NOT a valid local part\n@example.com" in
    assert_raises (InvalidAddress email) (fun () -> validate_email email)
let tests_validate_email = [validate_email_1; validate_email_2]

module V = ViewMock.Make(ModelMock)
module C = Make(ModelMock)(V)(MailMock)

let user_foo =
  ModelMock.{
    password = Some "blahblah";
    token = Some ("5A3B2F9D", Int64.max_int);
    alternative_email = Some "foo@example.org";
    admin = true
  }
let user_bar =
  ModelMock.{
    password = Some "blubb";
    token = None;
    alternative_email = None;
    admin = false
  }
let user_baz =
  ModelMock.{
    password = None;
    token = None;
    alternative_email = None;
    admin = false
  }
let user_frob =
  { user_baz with alternative_email = Some "frob@example.net" }

let session_foo_1 =
  ModelMock.{
    username = "foo";
    expires = Int64.max_int
  }
let auth_foo_1 =
  Model.{ auth_session = Some "foo1";
          auth_user = "foo";
          auth_level = Admin }
let session_foo_2 =
  ModelMock.{
    username = "foo";
    expires = Int64.min_int
  }
let session_baz =
  ModelMock.{
    username = "baz";
    expires = Int64.max_int
  }
let db_users =
  let open ModelMock in
  StringMap.empty
  |> StringMap.add "foo" user_foo
  |> StringMap.add "bar" user_bar
  |> StringMap.add "baz" user_baz
  |> StringMap.add "frob" user_frob
let db_sessions =
  let open ModelMock in
  StringMap.empty
  |> StringMap.add "foo1" session_foo_1
  |> StringMap.add "foo2" session_foo_2
  |> StringMap.add "baz" session_baz
let mk_model ?(users = []) ?(sessions = []) key =
  let model = ModelMock.{ db_users; db_sessions; key } in
  List.iter (fun (user, data) -> ModelMock.user_add user data model) users;
  List.iter (fun (sid, data) -> ModelMock.session_add sid data model) sessions;
  C.{ mailer = MailMock.create(); db = model }

let cmp_model
    ModelMock.{ db_users = users1; db_sessions = sessions1; key = key1 }
    ModelMock.{ db_users = users2; db_sessions = sessions2; key = key2 } =
  StringMap.equal (=) users1 users2
  && StringMap.equal (=) sessions1 sessions2
  && key1 = key2

let assert_no_mail ctrl =
  match !C.(ctrl.mailer) with
  | [] -> ()
  | l ->
    assert_failure
      (Format.asprintf "Got %d unexpected mails to %a"
         (List.length l)
         Fmt.(list string) (List.map fst l))

let test_event_login_login =
  "Test event_login with login" >:: fun ctx ->
    let open View in
    ModelMock.current_time := 200L;
    let model = mk_model ""
    and view =
      V.make ~login_operation:Login ~login_user:"bar"
        ~login_pass:"blubb" () in
    C.event_login model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model"
      (mk_model "" ~sessions:[
          "#0",
          ModelMock.{ username = "bar";
                      expires = 500L}
        ]).db
      model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      (Some "#0") (view.session);
    let session = Model.{ auth_session = Some "#0";
                          auth_user = "bar"; auth_level = User } in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model = model.db; session; messages = [] }] view.history;
    assert_no_mail model

let test_event_login_wrong_password =
  "Test event_login with login, wrong password" >:: fun ctx ->
    let open View in
    let model = mk_model ""
    and view =
      V.make ~login_operation:Login ~login_user:"bar"
        ~login_pass:"blubber" () in
    C.event_login model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" (mk_model "").db model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      None (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model = model.db; message = View.LoginFailed }] view.history;
    assert_no_mail model


let test_event_login_login_no_such_user =
  "Test event_login with login, unknown user" >:: fun ctx ->
    let open View in
    let model = mk_model ""
    and view =
      V.make ~login_operation:Login ~login_user:"nope"
        ~login_pass:"blubber" () in
    C.event_login model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" (mk_model "").db model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      None (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model = model.db; message = View.LoginFailed }] view.history;
    assert_no_mail model


let pp_complex_mime_message pp msg =
  let buf = Buffer.create 4096 in
  Netmime_channels.write_mime_message
    (new Netchannels.output_buffer buf) msg;
  Fmt.string pp (Buffer.contents buf)
let pp_mail pp (rcpt, msg) =
  Format.fprintf pp "@[<v>To: %s@,%a@]"
    rcpt pp_complex_mime_message msg

let assert_mail ?msg ~rcpt ~subject ~body (rcpt', ((header, _) as mail)) =
  let msg_prefix = match msg with Some msg -> msg ^ ": " | None -> "" in
  let buf = Buffer.create 4096 in
  Netmime_channels.write_mime_message ~wr_header:false
    (new Netchannels.output_buffer buf) mail;
  let body' = Buffer.contents buf in
  assert_equal ~msg:(msg_prefix ^ "body")
    ~pp_diff:(vs @@ Fmt.string) (String.trim body) (String.trim body');
  assert_equal ~msg:(msg_prefix ^ "rcpt")
    ~pp_diff:(vs @@ Fmt.string) rcpt rcpt';
  assert_equal ~msg:(msg_prefix ^ "subject")
    ~pp_diff:(vs @@ Fmt.string) subject (header # field "Subject")

let assert_one_mail C.{ mailer } ?msg ~rcpt ~subject ~body () =
  match !mailer with
  | [mail] -> assert_mail ?msg ~rcpt ~subject ~body mail
  | [] -> assert_failure "No message where one was expected"
  | l -> assert_failure
           (Format.sprintf "Expected one message, got %d" (List.length l))

let test_event_login_forgot_ok =
  "Test event_login with forgot" >:: fun ctx ->
    TestTools.set_up ctx;
    ModelMock.current_time := 100L;
    let model = mk_model ""
    and view =
      V.make ~login_operation:Forgot ~login_user:"frob" () in
    C.event_login model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model"
      (mk_model "" ~users:[
          "frob", { user_frob with token = Some ("frob:100", 600L) }
        ]).db
      model.db;
    assert_equal ~msg:"Session"
      ~pp_diff:(vs @@ Fmt.(option string)) None (view.session);
    assert_equal ~msg:"History"
      ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model = model.db; message = TokenSent "frob" }] view.history;
    assert_one_mail model ~rcpt:"frob@example.net" ~subject:"Forgotten password"
      ~body:"frob:100" ()

let test_event_login_forgot_no_email =
  "Test event_login with forgot, no registered e-mail" >:: fun ctx ->
    TestTools.set_up ctx;
    ModelMock.current_time := 100L;
    let model = mk_model ""
    and view =
      V.make ~login_operation:Forgot ~login_user:"bar" () in
    C.event_login model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model"
      (mk_model "" ~users:[
          "bar", { user_bar with token = Some ("bar:100", 600L) }
        ]).db
      model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string)) None (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model = model.db; message = TokenSent "bar" }] view.history;
    assert_one_mail ~rcpt:("bar@" ^ Config.((get()).mail_domain))
      ~subject:"Forgotten password"
      ~body:"bar:100" model ()

let test_event_login_forgot_no_user =
  "Test event_login with forgot, no such user" >:: fun ctx ->
    TestTools.set_up ctx;
    ModelMock.current_time := 100L;
    let model = mk_model ""
    and view =
      V.make ~login_operation:Forgot ~login_user:"argh" () in
    C.event_login model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model"
      (mk_model "").db
      model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string)) None (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model = model.db; message = TokenSent "argh" }] view.history;
    assert_no_mail model

let tests_login =
  [test_event_login_login; test_event_login_wrong_password;
   test_event_login_login_no_such_user;
   test_event_login_forgot_ok; test_event_login_forgot_no_email;
   test_event_login_forgot_no_user
  ]

let test_event_admin_logout =
  "Test event_admin with logout" >:: fun ctx ->
    let model = mk_model ""
    and view =
      V.make ~admin_operation:Logout ~session:"foo1" () in
    C.event_admin model view;
    let logout_model = mk_model "" in
    ModelMock.session_rem "foo1" logout_model.db;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db logout_model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string)) None (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model = model.db; message = NoMessage }] view.history;
    assert_no_mail model

let test_event_admin_set_password =
  "Test event_admin with set_password" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:SetPass ~session:"foo1" ()
        ~admin_chpass_pass1:"pw1" ~admin_chpass_pass2:"pw1" in
    C.event_admin model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db
      (mk_model
         ~users:["foo",
                 ModelMock.{ user_foo with password = Some "pw1" }]
         "").db;
    let messages = View.[SUpdPassword "foo"] in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_no_mail model

let test_event_admin_set_password_mismatch =
  "Test event_admin with set_password" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:SetPass ~session:"foo1" ()
        ~admin_chpass_pass1:"pw1" ~admin_chpass_pass2:"pw2" in
    C.event_admin model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db (mk_model "").db;
    let messages = View.[FPasswordMismatch] in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_no_mail model

let test_event_admin_set_email =
  "Test event_admin with set_email" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:SetMail ~session:"foo1" ()
        ~admin_chmail_mail:"a@b.nu" in
    C.event_admin model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db
      (mk_model
         ~users:["foo",
                 ModelMock.{ user_foo with alternative_email = Some "a@b.nu" }]
         "").db;
    let messages = View.[SUpdEMail { user = "foo"; mail = Some "a@b.nu"} ] in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_no_mail model

let test_event_admin_set_email_empty =
  "Test event_admin with set_email" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:SetMail ~session:"foo1" () in
    C.event_admin model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db
      (mk_model
         ~users:["foo",
                 ModelMock.{ user_foo with alternative_email = None }]
         "").db;
    let messages = View.[SUpdEMail { user = "foo"; mail = None } ] in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_no_mail model

let test_event_admin_delete =
  "Test event_admin with delete" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:Delete ~session:"foo1" ()
        ~admin_delete_confirm:true in
    C.event_admin model view;
    let model' = mk_model "" in
    ModelMock.user_rem "foo" model'.db;
    ModelMock.session_rem "foo1" model'.db;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model'.db model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model = model.db; message = NoMessage }] view.history;
    assert_no_mail model

let test_event_admin_delete_unconfirmed =
  "Test event_admin with delete, unconfirmed" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:Delete ~session:"foo1" () in
    C.event_admin model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db (mk_model "").db;
    let messages = View.[FDeleteNotConfirmed "foo"] in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_no_mail model

let test_event_admin_create_nopw_nomail =
  "Test event_admin with create, no password, no mail" >:: fun ctx ->
    ModelMock.current_time := 200L;
    let model= mk_model ""
    and view =
      V.make ~admin_operation:Create ~session:"foo1"
        ~admin_create_user:"xyz" ~admin_create_level:User
        () in
    C.event_admin model view;
    let token = "xyz:200" in
    let user_data = ModelMock.{
        password = None;
        token = Some (token, 700L);
        alternative_email = None;
        admin = false
      } in
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db (mk_model ~users:["xyz", user_data] "").db;
    let messages =
      View.[SCreatedUserWithToken { user="xyz"; level = User; token }]
    in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_no_mail model

let test_event_admin_create_nopw_mail =
  "Test event_admin with create, no password, mail" >:: fun ctx ->
    TestTools.set_up ctx;
    ModelMock.current_time := 200L;
    let model= mk_model ""
    and view =
      V.make ~admin_operation:Create ~session:"foo1"
        ~admin_create_user:"xyz" ~admin_create_level:Admin
        ~admin_create_mail:"u@v.dd"
        () in
    C.event_admin model view;
    let token = "xyz:200" in
    let user_data = ModelMock.{
        password = None;
        token = Some (token, 700L);
        alternative_email = Some "u@v.dd";
        admin = true
      } in
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db (mk_model ~users:["xyz", user_data] "").db;
    let messages =
      View.[SCreatedUserSentToken { user="xyz"; level = Admin;
                                    mail = "u@v.dd" }]
    in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_one_mail ~rcpt:"u@v.dd" ~subject:"Your new account"
      ~body:token model ()

let test_event_admin_create_pw =
  "Test event_admin with create, password" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:Create ~session:"foo1"
        ~admin_create_user:"xyz" ~admin_create_level:User
        ~admin_create_pass:"abc"
        () in
    C.event_admin model view;
    let user_data = ModelMock.{
        password = Some "abc";
        token = None;
        alternative_email = None;
        admin = false
      } in
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db (mk_model ~users:["xyz", user_data] "").db;
    let messages =
      View.[SCreatedUser { user="xyz"; level = User }]
    in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_no_mail model

let test_event_admin_create_not_admin =
  "Test event_admin with create, not admin" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:Create ~session:"baz"
        ~admin_create_user:"xyz" ~admin_create_level:User
        ~admin_create_pass:"abc"
        () in
    assert_raises Model.(AuthorizationNeeded Admin)
      (fun () -> C.event_admin model view)

let test_event_admin_mass_update_not_admin =
  "Test event_admin with mass update, not admin" >:: fun ctx ->
    let model= mk_model ""
    and view =
      V.make ~admin_operation:MassUpdate ~session:"baz"
        ~admin_mass_update:[] () in
    assert_raises Model.(AuthorizationNeeded Admin)
      (fun () -> C.event_admin model view)

(* Only do minimal testing here - the cool stuff
   is tested in the model and view anyway! *)
let test_event_admin_mass_update =
  "Test event_admin with mass update" >:: fun ctx ->
    TestTools.set_up ctx;
    ModelMock.current_time := 900L;
    let model= mk_model ""
    and view =
      V.make ~admin_operation:MassUpdate ~session:"foo1"
        ~admin_mass_update:Model.[ TaskCreateToken "frob" ]
        () in
    C.event_admin model view;
    let user_data = { user_frob with token = Some ("frob:900", 1400L) } in
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" model.db (mk_model ~users:["frob", user_data] "").db;
    let messages =
      View.[SSentToken "frob"]
    in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; session=auth_foo_1; messages }] view.history;
    assert_one_mail ~rcpt:"frob@example.net" ~subject:"Forgotten password"
      ~body:"frob:900" model ()

let test_event_admin_no_session =
  "Test event_admin without session" >:: fun ctx ->
    let model = mk_model ""
    and view = V.make ~admin_operation:MassUpdate ~session:"junk" () in
    C.event_admin model view;
    assert_equal ~cmp:cmp_model ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" (mk_model "").db model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      (Some "junk") (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Login { model=model.db; message = View.LoginFailed }] view.history;
    assert_no_mail model

let test_event_forgot_nopw =
  "Test event_forgot without password" >:: fun ctx ->
    let model = mk_model ""
    and view = V.make ~forgot_user:"foo" ~forgot_token:"5A3B2F9D" () in
    C.event_forgot model view;
    assert_equal ~cmp:cmp_model  ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" (mk_model "").db model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      None (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Forgot { model=model.db; user = "foo"; token = "5A3B2F9D"; badpw=false }]
      view.history;
    assert_no_mail model

let test_event_forgot_withpw =
  "Test event_forgot with passwords" >:: fun ctx ->
    let model = mk_model ""
    and view =
      V.make ~forgot_user:"foo" ~forgot_token:"5A3B2F9D"
        ~forgot_pass1:"pass" ~forgot_pass2:"pass" () in
    C.event_forgot model view;
    assert_equal ~cmp:cmp_model  ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model"
      (mk_model
         ~users:["foo", { user_foo with password = Some "pass" }] "").db
      model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      None (view.session);
    let messages = View.[SUpdPassword "foo"] in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model=model.db; messages;
                 session={ auth_foo_1 with auth_session = None } }]
      view.history;
    assert_no_mail model

let test_event_forgot_withpw_mismatch =
  "Test event_forgot with passwords" >:: fun ctx ->
    let model = mk_model ""
    and view =
      V.make ~forgot_user:"foo" ~forgot_token:"5A3B2F9D"
        ~forgot_pass1:"pass" ~forgot_pass2:"pass'" () in
    C.event_forgot model view;
    assert_equal ~cmp:cmp_model  ~pp_diff:(vs ModelMock.pp_database)
      ~msg:"Model" (mk_model "").db model.db;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      None (view.session);
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Forgot { model=model.db; user = "foo"; token = "5A3B2F9D"; badpw=true }]
      view.history;
    assert_no_mail model

let tests_admin =
  [test_event_admin_logout;
   test_event_admin_set_password;
   test_event_admin_set_password_mismatch;
   test_event_admin_set_email;
   test_event_admin_set_email_empty;
   test_event_admin_delete;
   test_event_admin_delete_unconfirmed;
   test_event_admin_create_nopw_nomail;
   test_event_admin_create_nopw_mail;
   test_event_admin_create_pw;
   test_event_admin_create_not_admin;
   test_event_admin_mass_update_not_admin;
   test_event_admin_mass_update;
   test_event_admin_no_session;
   test_event_forgot_nopw;
   test_event_forgot_withpw;
   test_event_forgot_withpw_mismatch
  ]


let tests =
  "Controller" >:::
  (tests_validate_user @
   tests_validate_pass @
   tests_validate_email @
   tests_login @ tests_admin
  )

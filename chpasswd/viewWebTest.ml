open Kaputt
open View

module V = ViewWeb.Make(ModelMock)(WebMock)
module SM = WebMock.StringMap
module A = TestAssertions.Assertion

let session_id = "sid1"
let make ?session params =
  WebMock.make ?session
    (List.fold_left (fun params (k,v) -> SM.add k v params)
       SM.empty params)

let user = "foo"
let pass = "bar"
let pass2 = "bar2"
let mail = "foo@example.net"
let request_junk = make [("operation", "complete junk for testing")]
let request_login_noop = make []
let request_login_login =
  make [("operation", "login"); ("user", user); ("pass", pass)]
let request_login_forgot =
  make [("operation", "forgot"); ("user", user)]
let request_admin_noop =
  make ~session:session_id []
let request_admin_logout =
  make ~session:session_id [("operation", "logout")]
let request_admin_set_pass =
  make ~session:session_id
    [("operation", "set_pass"); ("pass1", pass); ("pass2", pass2)]
let request_admin_set_mail_set =
  make ~session:session_id
    [("operation", "set_mail"); ("mail", mail)]
let request_admin_set_mail_unset =
  make ~session:session_id
    [("operation", "set_mail")]
let request_admin_delete_on =
  make ~session:session_id
    [("operation", "delete"); ("confirm", "checked")]
let request_admin_delete_off =
  make ~session:session_id
    [("operation", "delete")]
let request_admin_create_with_pass_mail_admin =
  make ~session:session_id
    [("operation", "create"); ("user", user);
     ("pass", pass); ("mail", mail); ("admin", "checked")]
let request_admin_create_without_pass_mail_user =
  make ~session:session_id
    [("operation", "create"); ("user", user)]
let request_admin_mass_update =
  make ~session:session_id
    [("operation", "mass_update");
     ("user:foo", "checked");
     ("mktok:foo", "checked");
     ("omail:foo", mail);
     ("nmail:foo", mail);
     ("pass:foo", pass);
     ("olevel:foo", "checked");
     ("nlevel:foo", "checked");
     ("user:bar", "checked");
     ("rmtok:bar", "checked");
     ("user:baz", "checked");
     ("delete:baz", "checked");
     ("user:frob", "checked");
     ("pass:frob", "raerara3r");
     ("delete:frob", "checked");
     ("user:blah", "checked");
     ("pass:blah", pass2);
     ("nmail:blah", mail);
     ("nlevel:blah", "checked");
     ("user:blubb", "checked");
     ("omail:blubb", mail);
     ("olevel:blubb", "checked")
    ]
let request_forgot_no_pass =
  make [("user", user); ("token", session_id)]
let request_forgot_pass =
  make [("user", user); ("token", session_id);
	("pass1", pass); ("pass2", pass2)]

let test_get_login_operation =
  Test.make_simple_test ~title:"get_login_operation"
    (fun () ->
       A.equal ~msg:"noop" (NoOperation: login_operation)
	 (V.get_login_operation request_login_noop);
       A.equal ~msg:"login" Login (V.get_login_operation request_login_login);
       A.equal ~msg:"forgot" Forgot (V.get_login_operation request_login_forgot);
       A.raises (fun () -> V.get_login_operation request_junk))

let test_get_login_user =
  Test.make_simple_test ~title:"get_login_user"
    (fun () ->
       A.equal_string ~msg:"with" user (V.get_login_user request_login_login);
       A.raises ~msg:"without" (fun () -> V.get_login_user request_login_noop))


let test_get_login_pass =
  Test.make_simple_test ~title:"get_login_pass"
    (fun () ->
       A.equal_string ~msg:"with" pass (V.get_login_pass request_login_login);
       A.raises ~msg:"without" (fun () -> V.get_login_pass request_login_noop))

let test_get_admin_sessionid =
  Test.make_simple_test ~title:"get_admin_sessionid"
    (fun () ->
       A.equal_string ~msg:"with" session_id (V.get_admin_sessionid request_admin_noop);
       A.raises ~msg:"without" (fun () -> V.get_admin_sessionid request_login_noop))

let test_get_admin_operation =
  Test.make_simple_test ~title:"get_admin_operation"
    (fun () ->
       A.equal ~msg:"noop" (NoOperation: admin_operation)
	 (V.get_admin_operation request_admin_noop);
       A.equal ~msg:"logout" Logout (V.get_admin_operation request_admin_logout);
       A.equal ~msg:"set_pass" SetPass (V.get_admin_operation request_admin_set_pass);
       A.equal ~msg:"set_mail" SetMail (V.get_admin_operation request_admin_set_mail_set);
       A.equal ~msg:"delete" Delete (V.get_admin_operation request_admin_delete_on);
       A.equal ~msg:"create" Create
	 (V.get_admin_operation request_admin_create_with_pass_mail_admin);
       A.equal ~msg:"mass_udpate" MassUpdate (V.get_admin_operation request_admin_mass_update);
       A.raises (fun () -> V.get_admin_operation request_junk))

let test_get_admin_chpass_pass1 =
  Test.make_simple_test ~title:"get_chpass_pass1"
    (fun () ->
       A.equal_string ~msg:"with" pass (V.get_admin_chpass_pass1 request_admin_set_pass);
       A.raises ~msg:"without" (fun () -> V.get_admin_chpass_pass1 request_login_noop))

let test_get_admin_chpass_pass2 =
  Test.make_simple_test ~title:"get_chpass_pass2"
    (fun () ->
       A.equal_string ~msg:"with" pass2 (V.get_admin_chpass_pass2 request_admin_set_pass);
       A.raises ~msg:"without" (fun () -> V.get_admin_chpass_pass2 request_login_noop))

let test_get_admin_chmail_mail =
  Test.make_simple_test ~title:"get_chmail_mail"
    (fun () ->
       A.equal_string_opt ~msg:"with" (Some mail)
	 (V.get_admin_chmail_mail request_admin_set_mail_set);
       A.equal_string_opt ~msg:"without" None
	 (V.get_admin_chmail_mail request_admin_set_mail_unset))

let test_get_admin_delete_confirm =
  Test.make_simple_test ~title:"get_delete_confirm"
    (fun () ->
       A.equal_bool ~msg:"with" true
	 (V.get_admin_delete_confirm request_admin_delete_on);
       A.equal_bool ~msg:"without" false
	 (V.get_admin_delete_confirm request_admin_delete_off))

let test_get_admin_create_user =
  Test.make_simple_test ~title:"get_admin_create_user"
    (fun () ->
       A.equal_string ~msg:"with" user
	 (V.get_admin_create_user request_admin_create_with_pass_mail_admin);
       A.raises ~msg:"without" (fun () -> V.get_admin_create_user request_login_noop))

let test_get_admin_create_pass =
  Test.make_simple_test ~title:"get_create_pass"
    (fun () ->
       A.equal_string_opt ~msg:"with" (Some pass)
	 (V.get_admin_create_pass request_admin_create_with_pass_mail_admin);
       A.equal_string_opt ~msg:"without" None
	 (V.get_admin_create_pass request_admin_create_without_pass_mail_user))

let test_get_admin_create_mail =
  Test.make_simple_test ~title:"get_create_mail"
    (fun () ->
       A.equal_string_opt ~msg:"with" (Some mail)
	 (V.get_admin_create_mail request_admin_create_with_pass_mail_admin);
       A.equal_string_opt ~msg:"without" None
	 (V.get_admin_create_mail request_admin_create_without_pass_mail_user))

let test_get_admin_create_level =
  Test.make_simple_test ~title:"get_create_level"
    (fun () ->
       A.equal ~msg:"with" Model.Admin
	 (V.get_admin_create_level request_admin_create_with_pass_mail_admin);
       A.equal ~msg:"without" Model.User
	 (V.get_admin_create_level request_admin_create_without_pass_mail_user))

let test_get_admin_create_pass =
  Test.make_simple_test ~title:"get_create_pass"
    (fun () ->
       A.equal_string_opt ~msg:"with" (Some pass)
	 (V.get_admin_create_pass request_admin_create_with_pass_mail_admin);
       A.equal_string_opt ~msg:"without" None
	 (V.get_admin_create_pass request_admin_create_without_pass_mail_user))

let test_get_admin_create_pass1 =
  Test.make_simple_test ~title:"get_create_pass1"
    (fun () ->
       A.equal_string_opt ~msg:"with" (Some pass)
	 (V.get_forgot_pass1 request_forgot_pass);
       A.equal_string_opt ~msg:"without" None
	 (V.get_forgot_pass1 request_forgot_no_pass))

let test_get_admin_create_pass2 =
  Test.make_simple_test ~title:"get_create_pass2"
    (fun () ->
       A.equal_string_opt ~msg:"with" (Some pass2)
	 (V.get_forgot_pass2 request_forgot_pass);
       A.equal_string_opt ~msg:"without" None
	 (V.get_forgot_pass2 request_forgot_no_pass))

let test_get_forgot_user =
  Test.make_simple_test ~title:"get_forgot_user"
    (fun () ->
       A.equal_string ~msg:"with" user (V.get_forgot_user request_forgot_pass);
       A.raises ~msg:"without" (fun () -> V.get_forgot_user request_junk))

let test_get_forgot_token =
  Test.make_simple_test ~title:"get_forgot_user"
    (fun () ->
       A.equal_string ~msg:"with" session_id (V.get_forgot_token request_forgot_pass);
       A.raises ~msg:"without" (fun () -> V.get_forgot_token request_junk))

let test_get_admin_mass_update =
  Test.make_simple_test ~title:"get_admin_mass_update"
    (fun () ->
       let ops = V.get_admin_mass_update request_admin_mass_update in
       (* Expected operations:
	  foo: create token
	  bar: delete token
	  baz: delete
	  frob: delete
	  blah: set_pass pass2, set_mail mail, set_admin admin
	  blubb: set_mail None, set_admin user *)
       (*A.equal_int 9 (List.length ops);*)
       let has what = A.is_true (List.mem what ops) in
       let open Model in
       has (TaskCreateToken "foo");
       has (TaskDeleteToken "bar");
       has (TaskDelete "baz");
       has (TaskDelete "frob");
       has (TaskSetPassword { user="blah"; pass=pass2 });
       has (TaskSetEMail { user="blah"; mail = Some mail });
       has (TaskSetAdmin { user="blah"; level = Admin });
       has (TaskSetEMail { user="blubb"; mail = None });
       has (TaskSetAdmin { user="blubb"; level = User });
       A.is_false (List.exists (function
	   | TaskSetPassword { user="frob" } -> true
	   | _ -> false) ops))

let test_view_open_session =
  Test.make_simple_test ~title:"view_open_session"
    (fun () ->
       let view = make [] in
       A.is_none view.session;
       V.view_open_session view session_id;
       A.equal_string_opt (Some session_id) view.session)

let test_view_close_session =
  Test.make_simple_test ~title:"view_close_session"
    (fun () ->
       let view = make ~session:session_id [] in
       A.equal_string_opt (Some session_id) view.session;
       V.view_close_session view;
       A.is_none view.session)

let user_data =
  ModelMock.{ password = Some pass; token = None;
	      alternative_email = Some mail; admin = true }
let user2 = "bar"
let user2_data =
  ModelMock.{ password = Some pass2; token = Some (session_id, 1.0);
	      alternative_email = None; admin = false }
let model =
  ModelMock.{ db_sessions = StringMap.empty;
	      db_users = StringMap.empty
		  |> StringMap.add user user_data
                  |> StringMap.add user2 user2_data }

let login_template =
  "{% if (message is undefined) %}nothing" ^
  "{% elseif message.key == \"login_failed\" %}login_failed" ^
  "{% else %}forgot:{{message.user}}{% endif %}"
let test_view_login =
  Test.make_assert_test ~title:"view_login"
    (fun () ->
       let dir = DatabaseTestTools.setup_tmpdir () in
       Config.datadir := dir; dir)
    (fun dir ->
       let view = make [] in
       let chan = open_out (Filename.concat dir "login.html") in
       output_string chan login_template;
       close_out chan;
       V.view_login model view View.NoMessage;
       A.equal_string_opt (Some "nothing") view.page_body;
       dir)
    DatabaseTestTools.delete_tmpdir

let test_view_login_token_sent =
  Test.make_assert_test ~title:"view_login, token sent"
    (fun () ->
       let dir = DatabaseTestTools.setup_tmpdir () in
       Config.datadir := dir; dir)
    (fun dir ->
       let view = make [] in
       let chan = open_out (Filename.concat dir "login.html") in
       output_string chan login_template;
       close_out chan;
       V.view_login model view (View.TokenSent user);
       A.equal_string_opt (Some ("forgot:" ^ user)) view.page_body;
       dir)
    DatabaseTestTools.delete_tmpdir

let test_view_login_failed =
  Test.make_assert_test ~title:"view_login, login failed"
    (fun () ->
       let dir = DatabaseTestTools.setup_tmpdir () in
       Config.datadir := dir; dir)
    (fun dir ->
       let view = make [] in
       let chan = open_out (Filename.concat dir "login.html") in
       output_string chan login_template;
       close_out chan;
       V.view_login model view View.LoginFailed;
       A.equal_string_opt (Some ("login_failed")) view.page_body;
       dir)
    DatabaseTestTools.delete_tmpdir

let admin_user_template =
  "user={{user}};email={%if alt_email is null%}(none){%else%}{{alt_email}};" ^
  "{%for msg in infos%}info:{{msg}};{%endfor%}" ^
  "{%for msg in errors%}erros:{{msg}};{%endfor%}" 
let admin_admin_template =
  "user={{user}};email={%if alt_email is null%}(none){%else%}{{alt_email}};" ^
  "{%for msg in infos%}info:{{msg}};{%endfor%}" ^
  "{%for msg in errors%}error:{{msg}};{%endfor%}" ^
  "{%for user in users%}user:{{user}};{%endfor%}"

let expected_view_admin_user_all_messages = ""
let expected_view_admin_admin_all_messages = ""

let test_view_admin_user_all_messages =
  Test.make_assert_test ~title:"view_admin, user"
    (fun () ->
       let dir = DatabaseTestTools.setup_tmpdir () in
       Config.datadir := dir; dir)
    (fun dir ->
       let view = make [] in
       let chan = open_out (Filename.concat dir "admin_user.html") in
       output_string chan admin_user_template;
       close_out chan;
       V.view_admin model view
	 { auth_session = Some session_id; auth_user = user;
	   auth_level = User }
	 [SUpdPassword user; SUpdEMail {user; mail=Some mail};
	  SUpdEMail {user; mail=None};
	  SUpdAdmin {user; level = User};
	  SUpdAdmin {user; level = Admin};
	  SSentToken user;
	  SDeletedToken user;
	  SCreatedUser {user; level = User};
	  SCreatedUserSentToken {user; mail; level = Admin};
	  SCreatedUserWithToken {user; token = session_id; level = User};
	  SDeletedUser user;
	  FDatabase "dberr";
	  FExternal "exterr";
	  FDeleteAllAdmin;
	  FDeleteCurrent;
	  FDeleteNotConfirmed user;
	  FPasswordMismatch;
	  FAuth Model.User;
	  FAuth Model.Admin];
       A.equal_string_opt (Some expected_view_admin_user_all_messages) view.page_body;
       dir)
    DatabaseTestTools.delete_tmpdir

let test_view_admin_admin_all_messages =
  Test.make_assert_test ~title:"view_admin, admin"
    (fun () ->
       let dir = DatabaseTestTools.setup_tmpdir () in
       Config.datadir := dir; dir)
    (fun dir ->
       let view = make [] in
       let chan = open_out (Filename.concat dir "admin_admin.html") in
       output_string chan admin_admin_template;
       close_out chan;
       V.view_admin model view
	 { auth_session = Some session_id; auth_user = user;
	   auth_level = Admin }
	 [SUpdPassword user; SUpdEMail {user; mail=Some mail};
	  SUpdEMail {user; mail=None};
	  SUpdAdmin {user; level = User};
	  SUpdAdmin {user; level = Admin};
	  SSentToken user;
	  SDeletedToken user;
	  SCreatedUser {user; level = User};
	  SCreatedUserSentToken {user; mail; level = Admin};
	  SCreatedUserWithToken {user; token = session_id; level = User};
	  SDeletedUser user;
	  FDatabase "dberr";
	  FExternal "exterr";
	  FDeleteAllAdmin;
	  FDeleteCurrent;
	  FDeleteNotConfirmed user;
	  FPasswordMismatch;
	  FAuth Model.User;
	  FAuth Model.Admin];
       A.equal_string_opt (Some expected_view_admin_admin_all_messages) view.page_body;
       dir)
    DatabaseTestTools.delete_tmpdir

let forgot_template =
  "user={{user}};token={{token}};pw_mismatch={{pw_mismatch}}"
let expected_view_forgot_form =
  "user=" ^ user ^ ";token=" ^ session_id ^ ";pw_mismatch=false"

let test_view_forgot_form =
  Test.make_assert_test ~title:"view_forgot_form"
   (fun () ->
       let dir = DatabaseTestTools.setup_tmpdir () in
       Config.datadir := dir; dir)
    (fun dir ->
       let view = make [] in
       let chan = open_out (Filename.concat dir "forgot.html") in
       output_string chan forgot_template;
       close_out chan;
       V.view_forgot_form model view ~user ~token:session_id false;
       A.equal_string_opt (Some expected_view_forgot_form) view.page_body;
       dir)
    DatabaseTestTools.delete_tmpdir

 
let () = Test.run_tests
  [ test_get_login_operation; test_get_login_user; test_get_login_pass;
  test_get_admin_sessionid; test_get_admin_operation;
  test_get_admin_chpass_pass1; test_get_admin_chpass_pass2;
  test_get_admin_chmail_mail; test_get_admin_delete_confirm;
  test_get_admin_create_user; test_get_admin_create_pass;
  test_get_admin_create_mail; test_get_admin_create_level;
  test_get_admin_create_pass; test_get_admin_create_pass1;
  test_get_admin_create_pass2; test_get_forgot_user;
  test_get_forgot_token; test_get_admin_mass_update; test_view_open_session;
  test_view_close_session; test_view_login; test_view_login_token_sent;
  test_view_login_failed; test_view_admin_user_all_messages;
  test_view_admin_admin_all_messages; test_view_forgot_form ]

open OUnit2
open View

module V = ViewWeb.Make(ModelMock)(WebMock)
module SM = WebMock.StringMap

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

let assert_equal ?msg ~(fmt: 'a Fmt.t) (expected: 'a) (got: 'a) =
  assert_equal ?msg ~printer:(Fmt.to_to_string fmt) expected got

let assert_raises_some ?msg fn =
  try ignore (fn ()); assert_failure
      ((match msg with Some prefix -> prefix ^ ": " | None -> "") ^
       "Expected an exception to be raised")
  with _ -> ()

let operation_to_string = function
  | Login -> "login"
  | Forgot -> "forgot"
  | NoOperation -> "no operation"
let pp_login_operation = Fmt.of_to_string operation_to_string

let test_get_login_operation =
  "get_login_operation" >:: fun ctx ->
    assert_equal ~fmt:pp_login_operation ~msg:"noop"
      (NoOperation: login_operation)
      (V.get_login_operation request_login_noop);
    assert_equal ~fmt:pp_login_operation ~msg:"login"
      Login (V.get_login_operation request_login_login);
    assert_equal ~fmt:pp_login_operation ~msg:"forgot"
      Forgot (V.get_login_operation request_login_forgot);
    assert_raises_some (fun () -> V.get_login_operation request_junk)

let test_get_login_user =
  "get_login_user" >:: fun ctx ->
    assert_equal ~fmt:Fmt.string ~msg:"with"
      user (V.get_login_user request_login_login);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_login_user request_login_noop)

let test_get_login_pass =
  "get_login_pass" >:: fun ctx ->
    assert_equal ~msg:"with" ~fmt:Fmt.string
      pass (V.get_login_pass request_login_login);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_login_pass request_login_noop)

let test_get_admin_sessionid =
  "get_admin_sessionid" >:: fun ctx ->
    assert_equal ~fmt:Fmt.string ~msg:"with"
      session_id (V.get_admin_sessionid request_admin_noop);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_admin_sessionid request_login_noop)

let admin_operation_to_string = function
  | Logout -> "logout"
  | SetPass -> "set_pass"
  | SetMail -> "set_mail"
  | Delete -> "delete"
  | Create -> "create"
  | MassUpdate -> "mass_update"
  | NoOperation -> "no_operation"
let pp_admin_operation = Fmt.of_to_string admin_operation_to_string

let test_get_admin_operation =
  "get_admin_operation" >:: fun ctx ->
    assert_equal ~fmt:pp_admin_operation ~msg:"noop"
      (NoOperation: admin_operation)
      (V.get_admin_operation request_admin_noop);
    assert_equal ~fmt:pp_admin_operation ~msg:"logout"
      Logout (V.get_admin_operation request_admin_logout);
    assert_equal ~fmt:pp_admin_operation ~msg:"set_pass"
      SetPass (V.get_admin_operation request_admin_set_pass);
    assert_equal ~fmt:pp_admin_operation ~msg:"set_mail"
      SetMail (V.get_admin_operation request_admin_set_mail_set);
    assert_equal ~fmt:pp_admin_operation ~msg:"delete"
      Delete (V.get_admin_operation request_admin_delete_on);
    assert_equal ~fmt:pp_admin_operation ~msg:"create"
      Create
      (V.get_admin_operation request_admin_create_with_pass_mail_admin);
    assert_equal ~fmt:pp_admin_operation ~msg:"mass_udpate"
      MassUpdate (V.get_admin_operation request_admin_mass_update);
    assert_raises_some (fun () -> V.get_admin_operation request_junk)

let test_get_admin_chpass_pass1 =
  "get_chpass_pass1" >:: fun ctx ->
    assert_equal ~fmt:Fmt.string ~msg:"with"
      pass (V.get_admin_chpass_pass1 request_admin_set_pass);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_admin_chpass_pass1 request_login_noop)

let test_get_admin_chpass_pass2 =
  "get_chpass_pass2" >:: fun ctx ->
    assert_equal ~fmt:Fmt.string ~msg:"with"
      pass2 (V.get_admin_chpass_pass2 request_admin_set_pass);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_admin_chpass_pass2 request_login_noop)

let test_get_admin_chmail_mail =
  "get_chmail_mail" >:: fun ctx ->
    assert_equal ~fmt:Fmt.(option string) ~msg:"with" (Some mail)
      (V.get_admin_chmail_mail request_admin_set_mail_set);
    assert_equal ~fmt:Fmt.(option string) ~msg:"without" None
      (V.get_admin_chmail_mail request_admin_set_mail_unset)

let test_get_admin_delete_confirm =
  "get_delete_confirm" >:: fun ctx ->
    assert_equal ~fmt:Fmt.bool ~msg:"with" true
      (V.get_admin_delete_confirm request_admin_delete_on);
    assert_equal ~fmt:Fmt.bool ~msg:"without" false
      (V.get_admin_delete_confirm request_admin_delete_off)

let test_get_admin_create_user =
  "get_admin_create_user" >:: fun ctx ->
    assert_equal ~fmt:Fmt.string ~msg:"with" user
      (V.get_admin_create_user request_admin_create_with_pass_mail_admin);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_admin_create_user request_login_noop)

let test_get_admin_create_pass =
  "get_create_pass" >:: fun ctx ->
    assert_equal ~fmt:Fmt.(option string) ~msg:"with" (Some pass)
      (V.get_admin_create_pass request_admin_create_with_pass_mail_admin);
    assert_equal ~fmt:Fmt.(option string) ~msg:"without" None
      (V.get_admin_create_pass request_admin_create_without_pass_mail_user)

let test_get_admin_create_mail =
  "get_create_mail" >:: fun ctx ->
    assert_equal ~fmt:Fmt.(option string) ~msg:"with" (Some mail)
      (V.get_admin_create_mail request_admin_create_with_pass_mail_admin);
    assert_equal ~fmt:Fmt.(option string) ~msg:"without" None
      (V.get_admin_create_mail request_admin_create_without_pass_mail_user)

let level_to_string = Model.(function Admin -> "admin" | User -> "user")
let pp_level = Fmt.of_to_string level_to_string

let test_get_admin_create_level =
  "get_create_level" >:: fun ctx ->
    assert_equal ~fmt:pp_level ~msg:"with" Model.Admin
      (V.get_admin_create_level request_admin_create_with_pass_mail_admin);
    assert_equal ~fmt:pp_level ~msg:"without" Model.User
      (V.get_admin_create_level request_admin_create_without_pass_mail_user)

let test_get_admin_create_pass =
  "get_create_pass" >:: fun ctx ->
    assert_equal ~fmt:Fmt.(option string) ~msg:"with" (Some pass)
      (V.get_admin_create_pass request_admin_create_with_pass_mail_admin);
    assert_equal ~fmt:Fmt.(option string) ~msg:"without" None
      (V.get_admin_create_pass request_admin_create_without_pass_mail_user)

let test_get_admin_create_pass1 =
  "get_create_pass1" >:: fun ctx ->
    assert_equal ~fmt:Fmt.(option string) ~msg:"with" (Some pass)
      (V.get_forgot_pass1 request_forgot_pass);
    assert_equal ~fmt:Fmt.(option string) ~msg:"without" None
      (V.get_forgot_pass1 request_forgot_no_pass)

let test_get_admin_create_pass2 =
  "get_create_pass2" >:: fun ctx ->
    assert_equal ~fmt:Fmt.(option string) ~msg:"with" (Some pass2)
      (V.get_forgot_pass2 request_forgot_pass);
    assert_equal ~fmt:Fmt.(option string) ~msg:"without" None
      (V.get_forgot_pass2 request_forgot_no_pass)

let test_get_forgot_user =
  "get_forgot_user" >:: fun ctx ->
    assert_equal ~fmt:Fmt.string ~msg:"with"
      user (V.get_forgot_user request_forgot_pass);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_forgot_user request_junk)

let test_get_forgot_token =
  "get_forgot_user" >:: fun ctx ->
    assert_equal ~fmt:Fmt.string ~msg:"with"
      session_id (V.get_forgot_token request_forgot_pass);
    assert_raises_some ~msg:"without"
      (fun () -> V.get_forgot_token request_junk)

let test_get_admin_mass_update =
  "get_admin_mass_update" >:: fun ctx ->
    let ops = V.get_admin_mass_update request_admin_mass_update in
    (* Expected operations:
       foo: create token
       bar: delete token
       baz: delete
       frob: delete
       blah: set_pass pass2, set_mail mail, set_admin admin
       blubb: set_mail None, set_admin user *)
    (*A.equal_int 9 (List.length ops);*)
    let has what =
      assert_bool "Missing list element" (List.mem what ops) in
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
    assert_bool "Extraneous list element"
      (not (List.exists (function
	   | TaskSetPassword { user="frob" } -> true
	   | _ -> false) ops))

let test_view_open_session =
  "view_open_session" >:: fun ctx ->
       let view = make [] in
       assert_equal ~fmt:Fmt.(option string) None view.session;
       V.view_open_session view session_id;
       assert_equal ~fmt:Fmt.(option string) (Some session_id) view.session

let test_view_close_session =
  "view_close_session" >:: fun ctx ->
       let view = make ~session:session_id [] in
       assert_equal ~fmt:Fmt.(option string) (Some session_id) view.session;
       V.view_close_session view;
       assert_equal ~fmt:Fmt.(option string) None view.session

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
  "view_login" >:: fun ctx ->
    let dir = bracket_tmpdir ctx in
    Config.(set_command_line datadir dir);
    let view = make [] in
    let chan = open_out (Filename.concat dir "login.html") in
    output_string chan login_template;
    close_out chan;
    V.view_login model view View.NoMessage;
    assert_equal ~fmt:Fmt.(option string) (Some "nothing") view.page_body

let test_view_login_token_sent =
  "view_login, token sent" >:: fun ctx ->
    let dir = bracket_tmpdir ctx in
    Config.(set_command_line datadir dir);
    let view = make [] in
    let chan = open_out (Filename.concat dir "login.html") in
    output_string chan login_template;
    close_out chan;
    V.view_login model view (View.TokenSent user);
    assert_equal ~fmt:Fmt.(option string) (Some ("forgot:" ^ user)) view.page_body

let test_view_login_failed =
  "view_login, login failed" >:: fun ctx ->
    let dir = bracket_tmpdir ctx in
    Config.(set_command_line datadir dir);
    let view = make [] in
    let chan = open_out (Filename.concat dir "login.html") in
    output_string chan login_template;
    close_out chan;
    V.view_login model view View.LoginFailed;
    assert_equal ~fmt:Fmt.(option string) (Some ("login_failed")) view.page_body

let admin_user_template =
  "user={{user}};email={% if alt_email is undefined%}(none){%else%}{{alt_email}}{% endif %};" ^
  "{%for msg in infos%}info:{{msg.key}};{%endfor%}" ^
  "{%for msg in errors%}error:{{msg.key}};{%endfor%}" 
let admin_admin_template =
  "user={{user}};email={% if alt_email is undefined%}(none){%else%}{{alt_email}}{% endif %};" ^
  "{%for msg in infos%}info:{{msg.key}};{%endfor%}" ^
  "{%for msg in errors%}error:{{msg.key}};{%endfor%}" ^
  "{%for user in users%}user:{{user.user}};{%endfor%}"

let expected_view_admin_user_all_messages = "user=foo;email=foo@example.net;info:user_deleted;info:created;info:created;info:created;info:token_deleted;info:token_sent;info:set_admin;info:set_user;info:upd_email;info:upd_email;info:upd_password;error:err_auth_admin;error:err_auth_user;error:err_pw_mismatch;error:err_delete_unconfirmed;error:err_delete_logged_in;error:err_delete_all_admin;error:err_ext;error:err_db;"
let expected_view_admin_admin_all_messages = "user=foo;email=foo@example.net;info:user_deleted;info:created;info:created;info:created;info:token_deleted;info:token_sent;info:set_admin;info:set_user;info:upd_email;info:upd_email;info:upd_password;error:err_auth_admin;error:err_auth_user;error:err_pw_mismatch;error:err_delete_unconfirmed;error:err_delete_logged_in;error:err_delete_all_admin;error:err_ext;error:err_db;user:bar;user:foo;"

let test_view_admin_user_all_messages =
  "view_admin, user" >:: fun ctx ->
    let dir = bracket_tmpdir ctx in
    Config.(set_command_line datadir dir);
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
    assert_equal ~fmt:Fmt.(option string)
      (Some expected_view_admin_user_all_messages) view.page_body

let test_view_admin_admin_all_messages =
  "view_admin, admin" >:: fun ctx ->
    let dir = bracket_tmpdir ctx in
    Config.(set_command_line datadir dir);
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
    assert_equal ~fmt:Fmt.(option string)
      (Some expected_view_admin_admin_all_messages) view.page_body

let forgot_template =
  "user={{user}};token={{token}};pw_mismatch={{pw_mismatch}}"
let expected_view_forgot_form =
  "user=" ^ user ^ ";token=" ^ session_id ^ ";pw_mismatch=false"

let test_view_forgot_form =
  "view_forgot_form" >:: fun ctx ->
  let dir = bracket_tmpdir ctx in
  Config.(set_command_line datadir dir);
  let view = make [] in
  let chan = open_out (Filename.concat dir "forgot.html") in
  output_string chan forgot_template;
  close_out chan;
  V.view_forgot_form model view ~user ~token:session_id false;
  assert_equal ~fmt:Fmt.(option string)
    (Some expected_view_forgot_form) view.page_body

let tests = "ViewWeb" >:::
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


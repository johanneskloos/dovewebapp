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
let user_frob = user_baz

let session_foo_1 =
  ModelMock.{
    username = "foo";
    expires = Int64.max_int
  }
let session_foo_2 =
  ModelMock.{
    username = "foo";
    expires = Int64.min_int
  }
let session_baz =
  ModelMock.{
    username = "baz";
    expires = Int64.min_int
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
let mk_model key = ModelMock.{ db_users; db_sessions; key }

let test_event_login_login =
  "Test event_login with login" >:: fun ctx ->
    let open View in
    let model = mk_model ""
    and view =
      V.make ~login_operation:Login ~login_user:"bar"
        ~login_pass:"blubb" () in
    C.event_login model view;
    assert_equal (mk_model "") model;
    assert_equal ~pp_diff:(vs @@ Fmt.(option string))
      (Some "tok1") (view.session);
    let session = Model.{ auth_session = Some "tok1";
                          auth_user = "bar"; auth_level = User } in
    assert_equal ~pp_diff:(vs @@ Fmt.list V.pp_history_item)
      V.[Admin { model; session; messages = [] }] view.history

(* TODO operation tests *)

let tests =
  "Controller - basic functionality" >:::
  (tests_validate_user @
   tests_validate_pass @
   tests_validate_email)

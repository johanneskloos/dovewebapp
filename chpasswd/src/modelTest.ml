open OUnit2
open Model

let auth_user =
  { auth_session = Some "blah"; auth_user = "foo"; auth_level = User }
let auth_admin =
  { auth_session = Some "blah"; auth_user = "foo"; auth_level = Admin }

let test_need_same_user =
  "need_same_user" >:: fun ctx ->
       need_same_user auth_user "foo";
       assert_raises (AuthorizationNeeded User)
	 (fun () -> need_same_user auth_user "bla");
       need_same_user auth_admin "foo";
       need_same_user auth_admin "bla"

let test_need_admin =
  "need_admin" >:: fun ctx ->
    assert_raises (AuthorizationNeeded Admin)
      (fun () -> need_admin auth_user);
    need_admin auth_admin

let tests = "Model" >::: [test_need_same_user; test_need_admin]

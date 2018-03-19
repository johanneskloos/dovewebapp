open Kaputt
open Model

let auth_user =
  { auth_session = Some "blah"; auth_user = "foo"; auth_level = User }
let auth_admin =
  { auth_session = Some "blah"; auth_user = "foo"; auth_level = Admin }

let test_need_same_user =
  Test.make_simple_test ~title:"need_same_user"
    (fun () ->
       let open Assertion in
       need_same_user auth_user "foo";
       raises (fun () -> need_same_user auth_user "bla");
       need_same_user auth_admin "foo";
       need_same_user auth_admin "bla")

let test_need_admin =
  Test.make_simple_test ~title:"need_admin"
    (fun () ->
       let open Assertion in
       raises (fun () -> need_admin auth_user);
       need_admin auth_admin)

let () = Test.run_tests [test_need_same_user; test_need_admin]

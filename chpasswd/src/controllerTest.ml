open Kaputt
open Controller
module A = Assertion

let validate_user_1 =
  Test.make_simple_test ~title:"Validate valid user name"
    (fun () ->
       let user = "Harry.Potter" in
       A.equal_string user (validate_user user))
let validate_user_2 =
  Test.make_simple_test ~title:"Validate invalid user name"
    (fun () ->
       A.raises (fun () -> validate_user "Harry\nPotter"))
let tests_validate_user = [validate_user_1; validate_user_2]

let validate_pass_1 =
  Test.make_simple_test ~title:"Validate valid password"
    (fun () ->
       let pass = "Expecto Patronum!" in
       A.equal_string pass (validate_pass pass))
let validate_pass_2 =
  Test.make_simple_test ~title:"Validate invalid password"
    (fun () ->
       A.raises (fun () -> validate_pass "Expecto\nPatronum!"))
let tests_validate_pass = [validate_pass_1; validate_pass_2]

let validate_email_1 =
  Test.make_simple_test ~title:"Validate valid e-mail address"
    (fun () ->
       let email = "harry.potter@hogwarts.ac.uk" in
       A.equal_string email (validate_email email))
let validate_email_2 =
  Test.make_simple_test ~title:"Validate invalid e-mail address"
    (fun () ->
       A.raises (fun () ->
	   validate_email "nope! - this is NOT a valid local part\n@example.com"))
let tests_validate_email = [validate_email_1; validate_email_2]

module V = ViewMock.Make(ModelMock)
module C = Make(ModelMock)(V)(MailMock)

(* TODO operation tests *)

let tests =
  tests_validate_user @
  tests_validate_pass @
  tests_validate_email

let () = Test.run_tests tests

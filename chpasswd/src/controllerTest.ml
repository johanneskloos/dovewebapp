open OUnit2
open Controller

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

(* TODO operation tests *)

let tests =
  "Controller - basic functionality" >:::
  (tests_validate_user @
   tests_validate_pass @
   tests_validate_email)

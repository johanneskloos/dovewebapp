open Kaputt

let make_equal_result eqok eqfail prok prfail =
  Assertion.make_equal (fun r1 r2 -> match r1, r2 with
      | Ok o1, Ok o2 -> eqok o1 o2
      | Error e1, Error e2 -> eqfail e1 e2
      | _, _ -> false)
    (function Ok o -> prok o | Error e -> prfail e)
let equal_string_result =
  make_equal_result (=) (=) (fun s -> "ok: " ^ s) (fun s -> "failure: " ^ s)

let result_failure ?msg result =
  Assertion.is_true ?msg (match result with Ok _ -> false | Error _ -> true)

let test_password_encode_1 =
  Test.make_simple_test ~title:"Encode a password with doveadm"
    (fun () ->
       equal_string_result
	 (Ok "{CRAM-MD5}6c872c7dd7cdf68f9392efce0ac212b1e361b8ce9acc5cf286a978b14755ec2d")
	 (Doveadm.password_encode ~user:"foo" ~pass:"bar"))

let test_password_encode_2 =
  Test.make_simple_test ~title:"Try to encode a password with a line break"
    (fun () ->
       result_failure ~msg:"Expected failure"
	 (Doveadm.password_encode ~user:"foo" ~pass:"bar\n"))

let test_password_encode_3 =
  Test.make_simple_test ~title:"Try to encode a password; user name has a line break"
    (fun () ->
       result_failure ~msg:"Expected failure"
	 (Doveadm.password_encode ~user:"foo\n" ~pass:"bar"))

let test_password_encode_4 =
  Test.make_simple_test ~title:"Try to encode a password; user name needs encoding"
    (fun () ->
       equal_string_result
	 (Ok "{CRAM-MD5}6c872c7dd7cdf68f9392efce0ac212b1e361b8ce9acc5cf286a978b14755ec2d")
  (Doveadm.password_encode ~user:"foo'; exit 1 #" ~pass:"bar"))

(* Tests for auth are surprisingly difficult to set up, don't do it here. *)

let () =
  Test.run_tests [
    test_password_encode_1;
    test_password_encode_2;
    test_password_encode_3;
    test_password_encode_4;
  ]

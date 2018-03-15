open Kaputt


let make_equal_result eqok eqfail prok prfail =
  Assertion.make_equal (fun r1 r2 -> match r1, r2 with
      | Ok o1, Ok o2 -> eqok o1 o2
      | Error e1, Error e2 -> eqfail e1 e2
      | _, _ -> false)
    (function Ok o -> prok o | Error e -> prfail e)
let equal_string_result =
  make_equal_result (=) (=) (fun s -> "ok: " ^ s) (fun s -> "failure: " ^ s)

let test_password_encode_1 =
  Test.make_simple_test ~title:"Encode a password with doveadm"
    (fun () ->
       equal_string_result
	 (Ok "{CRAM-MD5}6c872c7dd7cdf68f9392efce0ac212b1e361b8ce9acc5cf286a978b14755ec2d")
	 (Doveadm.password_encode ~user:"foo" ~pass:"bar"))

let () =
  Test.run_tests [
    test_password_encode_1
  ]

open OUnit2

let printer = Fmt.(to_to_string (result ~ok:string ~error:string))
let cmp res1 res2 =
  match res1, res2 with 
  | Ok val1, Ok val2 -> val1 = val2
  | Error _, Error _ -> true
  | _, _ -> false

let test_password_encode_1 =
  "Encode a password with doveadm" >:: fun ctx ->
    assert_equal ~cmp ~printer
      (Ok "{CRAM-MD5}6c872c7dd7cdf68f9392efce0ac212b1e361b8ce9acc5cf286a978b14755ec2d")
      (Doveadm.password_encode ~user:"foo" ~pass:"bar")

let test_password_encode_2 =
  "Try to encode a password with a line break" >:: fun ctx ->
    assert_equal ~cmp ~printer (Error "")
      (Doveadm.password_encode ~user:"foo" ~pass:"bar\n")

let test_password_encode_3 =
  "Try to encode a password; user name with a line break" >:: fun ctx ->
    assert_equal ~cmp ~printer (Error "")
      (Doveadm.password_encode ~user:"foo\n" ~pass:"bar")

let test_password_encode_4 =
  "Encode a password with doveadm; user name needs encoding" >:: fun ctx ->
    assert_equal ~cmp ~printer
      (Ok "{CRAM-MD5}6c872c7dd7cdf68f9392efce0ac212b1e361b8ce9acc5cf286a978b14755ec2d")
      (Doveadm.password_encode ~user:"foo'; exit 1 #" ~pass:"bar")

(* Tests for auth are surprisingly difficult to set up, don't do it here. *)

let tests = "Doveadm tests" >::: [
    test_password_encode_1;
    test_password_encode_2;
    test_password_encode_3;
    test_password_encode_4;
  ]

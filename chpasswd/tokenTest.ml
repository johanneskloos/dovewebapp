open Kaputt

let check_token_format =
  let open Specification in
  always ==> for_all_string is_digit_hex_char

let token_format_test =
  Test.make_random_test ~title:"generate" ~nb_runs:500
    Generator.unit Token.generate [check_token_format]

module StringSet = Set.Make(String)
let token_unique_test =
  Test.make_simple_test ~title:"generate uniqueness"
    (fun () ->
       let size = 1000 in
       let set = ref StringSet.empty in
       for i = 1 to size do
	 set := StringSet.add (Token.generate ()) !set;
       done;
       Assertion.equal_int ~msg:"Set size" size (StringSet.cardinal !set))

let () = Test.run_tests [token_format_test; token_unique_test]

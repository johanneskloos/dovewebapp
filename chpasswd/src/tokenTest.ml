let check_token_format token =
  try
    for i = 0 to String.length token - 1 do
      match String.unsafe_get token i with
      | 'A' .. 'F' | 'a' .. 'f' | '0' .. '9' -> ()
      | _ -> raise Exit
    done;
    true
  with Exit -> false


let token_format_test_qc =
  let open QCheck in
  Test.make ~count:500 ~name:"generate - format"
    unit (fun () -> check_token_format (Token.generate ()))

module StringSet = Set.Make(String)
let token_unique_test =
  let open OUnit2 in
  "generate uniqueness" >:: fun ctx ->
    let size = 1000 in
    let set = ref StringSet.empty in
    for i = 1 to size do
      set := StringSet.add (Token.generate ()) !set;
    done;
    assert_equal ~printer:string_of_int ~msg:"Set size"
      size (StringSet.cardinal !set)

let tests =
  let open OUnit2 in
  "Token" >::: [QCheck_runner.to_ounit_test token_format_test_qc |> OUnit.ounit2_of_ounit1;
		token_unique_test]

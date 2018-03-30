open OUnit2
open Template

let tests =
  "Template.from_file" >:: fun ctx ->
    TestTools.make_fake_templates ctx;
    assert_equal ~printer:Fmt.(to_to_string string)
      (String.trim "Test")
      (from_file ~models:[("token", Jg_types.Tstr "Test")]
         "forgot.822" |> String.trim)


open OUnit2
open Template

let tests =
  "Template.from_file" >:: fun ctx ->
    let dir = bracket_tmpdir ctx in
    let tmpl = Filename.concat dir "test.tmpl" in
    let chan = open_out tmpl in
    output_string chan "Test {{value}}";
    close_out chan;
    Config.(set_command_line datadir dir);
    assert_equal ~printer:Fmt.(to_to_string string)
      "Test 1"
      (from_file ~models:[("value", Jg_types.Tint 1)] "test.tmpl")


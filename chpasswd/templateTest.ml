open Kaputt
open Template

let test_from_file dir =
  Config.datadir := dir;
  let tmpl = Filename.concat dir "test.tmpl" in
  let chan = open_out tmpl in
  output_string chan "Test {{value}}";
  close_out chan;
  Assertion.equal_string "Test 1"
    (from_file ~models:[("value", Jg_types.Tint 1)] "test.tmpl");
  dir

let () = Test.run_test
    (Test.make_assert_test ~title:"from_file"
       DatabaseTestTools.setup_tmpdir
       test_from_file
       DatabaseTestTools.delete_tmpdir)

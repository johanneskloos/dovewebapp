open Jg_types

let make_environment () =
  { std_env with template_dirs = [ !Config.datadir ] }

let from_file ~models file =
  Jg_template.from_file ~env:(make_environment ()) ~models file

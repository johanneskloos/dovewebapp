open Jg_types

let make_environment () =
  { std_env with template_dirs = [ Config.((get()).path_templates) ] }

let from_file ~models file =
  Jg_template.from_file ~env:(make_environment ()) ~models file

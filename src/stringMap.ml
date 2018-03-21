include Map.Make(String)

let pp pp_val pp map =
  Fmt.iter_bindings ~sep:Fmt.(const string ", ") iter
    Fmt.(pair ~sep:(const string "=") string pp_val)
    pp map

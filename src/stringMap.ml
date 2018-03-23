include Map.Make(String)

let cstr = Fmt.(const string)

let pp pp_val pp map =
  (Fmt.hvbox @@
   Fmt.iter_bindings ~sep:Fmt.(prefix (cstr ",") sp) iter
     Fmt.(hvbox @@ pair ~sep:(prefix (cstr "=") cut)
            (hbox string) (box pp_val)))
    pp map

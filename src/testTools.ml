let vs pp = Fmt.(hvbox @@ (pair ~sep:(prefix (const string " vs.") sp) pp pp))
let assert_raises_some ?msg fn =
  try
    fn ();
    OUnit2.assert_failure
      ((match msg with Some prefix -> prefix ^ ": " | None -> "") ^
       "Expected an exception")
  with _ -> ()

              

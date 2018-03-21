let vs pp = Fmt.(pair ~sep:(const string " vs. ") pp pp)
let assert_raises_some ?msg fn =
  try
    fn ();
    OUnit2.assert_failure
      ((match msg with Some prefix -> prefix ^ ": " | None -> "") ^
       "Expected an exception")
  with _ -> ()

              

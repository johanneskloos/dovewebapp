let generate () =
  let open Cryptokit in
  let rand = Random.string Random.secure_rng 64
  and time = Time.current_string ()
  in hash_string (Hash.sha512 ()) (rand ^ time)

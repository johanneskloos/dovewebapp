open Cryptokit

let rng = Random.device_rng "/dev/urandom"
let hex_transform = Hexa.encode ()

let generate () =
  let rand = Random.string rng 64
  and time = Time.current_string ()
  in transform_string hex_transform
    (hash_string (Hash.sha512 ()) (rand ^ time))

type t = (string * Netmime.complex_mime_message) list ref

let create () = ref []

let send_mail mails rcpt body =
  mails := (rcpt, body) :: !mails

let mails: (string * Netmime.complex_mime_message) list ref = ref []

let send_mail rcpt body =
  mails := (rcpt, body) :: !mails

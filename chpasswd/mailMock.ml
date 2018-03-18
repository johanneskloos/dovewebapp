module MailMock = struct
  let mails: (string * Netmime.complex_mime_message) list ref = ref []

  let mails rcpt body =
    mails := (rcpt, body) :: !mails
end


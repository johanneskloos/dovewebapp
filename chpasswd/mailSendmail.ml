let send_mail address ((header, _) as message) =
  let open Netsmtp in
  header # update_field "To" address;
  let addr =
    `Socket (`Sock_inet_byname (Unix.SOCK_STREAM, Config.(get mailserver), 25),
	     Uq_client.default_connect_options) in
  let client = new Netsmtp.connect addr 10.0 in
  Netsmtp.sendmail client message

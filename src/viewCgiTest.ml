open OUnit2
open ViewCgi
open Netcgi
open TestTools

let make_cgi ?(cookies=[]) query buf =
  { cgi = make_fake_cgi ~cookies query buf; set_session = None }

let test_get_named_argument_opt =
  "get_named_argument_opt" >:: fun ctx ->
    let value = "blah"
    and field1 = "foo"
    and field2 = "bar"
    and outbuf = Buffer.create 1024 in
    let view = make_cgi [(field1, value)] outbuf in
    assert_equal ~pp_diff:Fmt.(vs @@ option string)
      (Some value) (get_named_argument_opt view field1);
    assert_equal ~pp_diff:Fmt.(vs @@ option string)
      None (get_named_argument_opt view field2)

let test_get_session_data =
  "get_session_data" >:: fun ctx ->
    let value = "xyz"
    and outbuf = Buffer.create 10 in
    assert_equal ~pp_diff:Fmt.(vs @@ option string) (Some value)
      (get_session_data (make_cgi ~cookies:[("session", value)]
                           [] outbuf));
    assert_equal ~pp_diff:Fmt.(vs @@ option string) None
      (get_session_data (make_cgi [] outbuf))

let extract buf =
  let (header, _) as msg =
    Buffer.contents buf |> Netchannels.input_string
    |> new Netstream.input_stream
    |> Netmime_channels.read_mime_message in
  Buffer.clear buf;
  Netmime_channels.write_mime_message ~wr_header:false
    (new Netchannels.output_buffer buf) msg;
  (header, Buffer.contents buf)

let netscape_cookie_eq c1 c2 =
  let open Nethttp in
  c1.cookie_name = c2.cookie_name && c1.cookie_value = c2.cookie_value

let pp_netscape_cookie pp Nethttp.{ cookie_name; cookie_value } =
  Format.fprintf pp "%s = %s" cookie_name cookie_value

let mkcookie cookie_name cookie_value =
  Nethttp.{ cookie_name; cookie_value; cookie_expires = None;
            cookie_domain = None; cookie_path = None;
            cookie_secure = false }

let test_output_page_ok_no_cookie =
  "output_page, ok, no cookie" >:: fun ctx ->
    let buf = Buffer.create 100 in
    let view = make_cgi [] buf
    and body = "blah" in
    output_page view ViewWeb.StatOk body;
    let (header, body') = extract buf in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "200" (String.sub (header # field "Status") 0 3);
    assert_equal ~pp_diff:(vs @@ Fmt.string) body body';
    let open Nethttp.Header in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "text/html" (fst (get_content_type header));
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_netscape_cookie)
      [] (get_set_cookie header);
    assert_equal ~pp_diff:Fmt.(vs @@ option string) None view.set_session


let test_output_page_ok_cookie =
  "output_page, ok, with cookie" >:: fun ctx ->
    let buf = Buffer.create 100 in
    let view = make_cgi [] buf
    and session = "xyz"
    and body = "blah" in
    view.set_session <- Some session;
    output_page view ViewWeb.StatOk body;
    let (header, body') = extract buf in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "200" (String.sub (header # field "Status") 0 3);
    assert_equal ~pp_diff:(vs @@ Fmt.string) body body';
    let open Nethttp.Header in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "text/html" (fst (get_content_type header));
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_netscape_cookie)
      [mkcookie "session" session] (get_set_cookie header);
    assert_equal ~pp_diff:Fmt.(vs @@ option string) None view.set_session

let test_output_page_auth =
  "output_page, auth error" >:: fun ctx ->
    let buf = Buffer.create 100 in
    let view = make_cgi [] buf
    and body = "blah" in
    output_page view ViewWeb.StatAuth body;
    let (header, body') = extract buf in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "401" (String.sub (header # field "Status") 0 3);
    assert_equal ~pp_diff:(vs @@ Fmt.string) body body';
    let open Nethttp.Header in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "text/html" (fst (get_content_type header));
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_netscape_cookie)
      [] (get_set_cookie header);
    assert_equal ~pp_diff:Fmt.(vs @@ option string) None view.set_session

let test_output_page_err =
  "output_page, generic error" >:: fun ctx ->
    let buf = Buffer.create 100 in
    let view = make_cgi [] buf
    and body = "blah" in
    output_page view ViewWeb.StatError body;
    let (header, body') = extract buf in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "500" (String.sub (header # field "Status") 0 3);
    assert_equal ~pp_diff:(vs @@ Fmt.string) body body';
    let open Nethttp.Header in
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "text/html" (fst (get_content_type header));
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_netscape_cookie)
      [] (get_set_cookie header);
    assert_equal ~pp_diff:Fmt.(vs @@ option string) None view.set_session


let tests =
  "ViewCgi" >:::
  [test_get_named_argument_opt; test_get_session_data;
   test_output_page_ok_no_cookie; test_output_page_ok_cookie;
   test_output_page_auth; test_output_page_err ]

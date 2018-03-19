open Kaputt
open ViewCgi
open Netcgi
module A = TestAssertions.Assertion

let make_cgi ?(cookies=[]) query buf =
  let open Netcgi_common in
  let encode_query = function
    | [] -> ""
    | (key, value) :: rest ->
      let buf = Buffer.create 256 in
      let add k v =
	Buffer.add_string buf (Nethttp.uripath_encode k);
	Buffer.add_string buf "=";
	Buffer.add_string buf (Nethttp.uripath_encode v) in
      add key value;
      let rec build = function
	| (key, value) :: rest ->
	  Buffer.add_string buf "&";
	  add key value;
	  build rest
	| [] -> Buffer.contents buf
      in build rest in
  let (properties, input_header) =
    List.fold_left (fun l e -> update_props_inheader e l)
      ([],[])
      ([ ("GATEWAY_INTERFACE", "CGI/1.1");
	("QUERY_STRING", encode_query query);
	("REMOTE_ADDR", "127.0.0.1");
	("REMOTE_HOST", "localhost");
	("REQUEST_METHOD", "GET");
	("SCRIPT_NAME", "test");
	("SERVER_NAME", "localhost");
	("SERVER_PORT", "80");
	("SERVER_PROTOCOL", "HTTP/1.1");
	("SERVER_SOFTWARE", "TestFake/1.0");
      ] @ List.map (fun (key, value) ->
	  ("HTTP_COOKIE", Nethttp.uripath_encode key ^ "=" ^
		     Nethttp.uripath_encode value)) cookies) in
  let cgi = cgi_with_args (new cgi)
    (new cgi_environment ~config:default_config ~input_header
      ~properties (new Netchannels.output_buffer buf))
    (`Direct "")
    (Netchannels.input_string "")
    (fun _ _ _ -> `Automatic)
  in { cgi; set_session = None }

let test_get_named_argument_opt =
  Test.make_simple_test ~title:"get_named_argument_opt"
    (fun () ->
       let value = "blah"
       and field1 = "foo"
       and field2 = "bar"
       and outbuf = Buffer.create 1024 in
       let view = make_cgi [(field1, value)] outbuf in
       A.equal_string_opt (Some value) (get_named_argument_opt view field1);
       A.is_none (get_named_argument_opt view field2))

let test_get_session_data =
  Test.make_simple_test ~title:"get_session_data"
    (fun () ->
       let value = "xyz"
       and outbuf = Buffer.create 10 in
       A.equal_string_opt (Some value)
	 (get_session_data (make_cgi ~cookies:[("session", value)] [] outbuf));
       A.is_none (get_session_data (make_cgi [] outbuf)))

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

let netscape_cookie_prn Nethttp.{ cookie_name; cookie_value } =
  Format.sprintf "%s = %s" cookie_name cookie_value

let mkcookie cookie_name cookie_value =
  Nethttp.{ cookie_name; cookie_value; cookie_expires = None;
	    cookie_domain = None; cookie_path = None;
	    cookie_secure = false }

let test_output_page_ok_no_cookie =
  Test.make_simple_test ~title:"output_page, ok, no cookie"
    (fun () ->
       let buf = Buffer.create 100 in
       let view = make_cgi [] buf
       and body = "blah" in
       output_page view ViewWeb.StatOk body;
       let (header, body') = extract buf in
       A.equal_string "200" (String.sub (header # field "Status") 0 3);
       A.equal_string body body';
       let open Nethttp.Header in
       A.equal_string_ignorecase "text/html" (fst (get_content_type header));
       A.make_equal (=) (A.list_prn netscape_cookie_prn)
	 [] (get_set_cookie header);
       A.is_none view.set_session
    )

let test_output_page_ok_cookie =
  Test.make_simple_test ~title:"output_page, ok, with cookie"
    (fun () ->
       let buf = Buffer.create 100 in
       let view = make_cgi [] buf
       and session = "xyz"
       and body = "blah" in
       view.set_session <- Some session;
       output_page view ViewWeb.StatOk body;
       let (header, body') = extract buf in
       A.equal_string "200" (String.sub (header # field "Status") 0 3);
       A.equal_string body body';
       let open Nethttp.Header in
       A.equal_string_ignorecase "text/html" (fst (get_content_type header));
       A.make_equal (=) (A.list_prn netscape_cookie_prn)
	 [mkcookie "session" session] (get_set_cookie header);
       A.is_none view.set_session)

let test_output_page_auth =
  Test.make_simple_test ~title:"output_page, auth error"
    (fun () ->
       let buf = Buffer.create 100 in
       let view = make_cgi [] buf
       and body = "blah" in
       output_page view ViewWeb.StatAuth body;
       let (header, body') = extract buf in
       A.equal_string "401" (String.sub (header # field "Status") 0 3);
       A.equal_string body body';
       let open Nethttp.Header in
       A.equal_string_ignorecase "text/html" (fst (get_content_type header));
       A.make_equal (=) (A.list_prn netscape_cookie_prn)
	 [] (get_set_cookie header);
       A.is_none view.set_session
    )

let test_output_page_err =
  Test.make_simple_test ~title:"output_page, generic error"
    (fun () ->
       let buf = Buffer.create 100 in
       let view = make_cgi [] buf
       and body = "blah" in
       output_page view ViewWeb.StatError body;
       let (header, body') = extract buf in
       A.equal_string "500" (String.sub (header # field "Status") 0 3);
       A.equal_string body body';
       let open Nethttp.Header in
       A.equal_string_ignorecase "text/html" (fst (get_content_type header));
       A.make_equal (=) (A.list_prn netscape_cookie_prn)
	 [] (get_set_cookie header);
       A.is_none view.set_session
    )


(* Add others later *)
let () =
  Test.run_tests
    [test_get_named_argument_opt; test_get_session_data;
     test_output_page_ok_no_cookie; test_output_page_ok_cookie;
     test_output_page_auth; test_output_page_err ]

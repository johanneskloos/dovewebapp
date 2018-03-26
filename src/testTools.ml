let vs pp = Fmt.(hvbox @@ (pair ~sep:(prefix (const string " vs.") sp) pp pp))
let assert_raises_some ?msg fn =
  try
    fn ();
    OUnit2.assert_failure
      ((match msg with Some prefix -> prefix ^ ": " | None -> "") ^
       "Expected an exception")
  with _ -> ()

let make_fake_cgi ?(path="/") ?(cookies=[]) query buf =
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
         ("PATH_INFO", path);
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
  cgi_with_args (new cgi)
    (new cgi_environment ~config:Netcgi.default_config ~input_header
      ~properties (new Netchannels.output_buffer buf))
    (`Direct "")
    (Netchannels.input_string "")
    (fun _ _ _ -> `Automatic)

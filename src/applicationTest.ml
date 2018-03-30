open OUnit2
open TestTools

let token_counter = ref 0
let now = ref 0L

let db_mutex = OUnitShared.Mutex.create OUnitShared.ScopeGlobal
let bracket_db_lock =
  OUnitBracket.create
    (fun ctx -> OUnitShared.Mutex.lock ctx.shared db_mutex)
    (fun () ctx -> OUnitShared.Mutex.unlock ctx.shared db_mutex)

module ExtImpl = struct
  let auth ~user ~pass =
    let db = Application.take_handle () in
    match
      Database.execute_select_at_most_one db
        "SELECT password FROM users WHERE username = ?"
        [Database.str user]
        (fun stmt -> Database.get_str stmt 0)
    with
    | Some pass' -> Application.drop_handle db; pass = pass'
    | None -> Application.drop_handle db; false
    | exception e -> Application.drop_handle db; raise e

  let password_encode ~user ~pass = Ok ("hash:" ^ pass)
  let generate_token () = 
    incr token_counter;
    "#" ^ string_of_int !token_counter
  let timestamp delay =
    Int64.(add !now (of_int delay))
end

type mail = { rcpt: string; body: string }
let pp_mail pp { rcpt; body } =
  Format.fprintf pp "%s -> %s" rcpt body

module MailStore = struct
  type t = mail list ref

  let create () = ref []

  let send_mail mails address mail =
    let buf = Buffer.create 1024 in
    Netmime_channels.write_mime_message
      (new Netchannels.output_buffer buf)
      mail;
    mails := { rcpt = address; body = Buffer.contents buf } :: !mails
end

module A = Application.Make(ExtImpl)(MailStore)

let db_setup =
  "INSERT INTO users VALUES (\"root\", \"t00r\", NULL, NULL, NULL, 1);" ^
  "INSERT INTO users VALUES (\"reader\", NULL, \"xyz\", 100, NULL, 1);" ^
  "INSERT INTO sessions VALUES (\"sid1\", 100, \"root\")"

type output = {
  mails: mail list;
  page: string
}

let run ctxt ?path ?cookies query =
  TestTools.make_fake_templates ctxt;
  TestTools.make_fake_database ~setup:db_setup ctxt;
  let buf = Buffer.create 4096 in
  let cgi = TestTools.make_fake_cgi ?path ?cookies query buf
  and mailer = MailMock.create () in
  A.handle_request ~mailer cgi;
  { mails = !mailer; page = Buffer.contents buf }

let assert_page ?(status = 200) body result =
  let stream = Netchannels.input_string result in
  match Netmime_channels.read_mime_message ~multipart_style:`None
          (new Netstream.input_stream stream) with
  | (header, `Body body') ->
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      (string_of_int status)
      (header # field "Status");
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "text/html" (header # field "Content-type");
    assert_equal ~pp_diff:(vs @@ Fmt.string)
      "no-cache" (header # field "Cache-control");
    assert_equal ~msg:"Page body" ~cmp:TestTools.streq_whitespace
      ~pp_diff:(vs @@ Fmt.(using TestTools.compact string))
      body (body' # value)
  | _ -> assert false

(** Only test some cases here - most of the interesting testing work
    has been done elsewhere already. *)
let test_login_login =
  "login page - log in root" >:: fun ctx ->
    let result = 
      run ctx [("operation", "login"); ("user", "root"); ("pass", "t00r")] in
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_mail) [] result.mails;
    let db = Application.take_handle () in
    Database.execute_select_one db
      "SELECT session_expires, username FROM sessions WHERE sessionid = '#1'"
      []
      (fun stmt ->
         assert_equal ~pp_diff:(vs @@ Fmt.int64)
           (Int64.of_int Config.((get()).lifetime_session))
           (Database.get_int64 stmt 0);
         assert_equal ~pp_diff:(vs @@ Fmt.string)
           "root"
           (Database.get_str stmt 1));
    Application.drop_handle db;
    assert_page
      ("\n\nuser:root\n" ^ "alt_email:(None)\n" ^
      "user:root:admin::user:reader:admin::xyz@1970-01-01 00:01:39\n")
      result.page

let test_admin_set_pass =
  "admin page - set password" >:: fun ctx ->
    let result =
      run ctx ~path:"/admin" ~cookies:["session", "sid1"]
        [("operation", "set_pass");
         ("pass1", "abc");
         ("pass2", "abc")] in
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_mail) [] result.mails;
    let db = Application.take_handle () in
    Database.execute_select_one db
      "SELECT password FROM users WHERE username = 'root'"
      []
      (fun stmt ->
         assert_equal ~pp_diff:(vs @@ Fmt.string)
           "hash:abc"
           (Database.get_str stmt 0));
    Application.drop_handle db;
    assert_page
      ("user:root\nalt_email:(None)\n\nsuccess:upd_password:root\n" ^
       " user:root:admin::user:reader:admin::xyz@1970-01-01 00:01:39")
      result.page

let test_admin_set_pass_fail =
  "admin page - set password, failed" >:: fun ctx ->
    let result =
      run ctx ~path:"/admin" ~cookies:["session", "sid1"]
        [("operation", "set_pass");
         ("pass1", "abc");
         ("pass2", "abd")] in
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_mail) [] result.mails;
    let db = Application.take_handle () in
    Database.execute_select_one db
      "SELECT password FROM users WHERE username = 'root'"
      []
      (fun stmt ->
         assert_equal ~pp_diff:(vs @@ Fmt.string)
           "t00r"
           (Database.get_str stmt 0));
    Application.drop_handle db;
    assert_page ("user:root\nalt_email:(None)\n" ^
                 "failure:err_pw_mismatch " ^
                 "user:root:admin::user:reader:admin::xyz@1970-01-01 00:01:39")
      result.page

let test_forgot_main =
  "forgot page - main" >:: fun ctx ->
    let result =
      run ctx ~path:"/forgot" [("user", "reader"); ("token", "xyz")] in
    assert_equal ~pp_diff:(vs @@ Fmt.list pp_mail) [] result.mails;
    assert_page "reader:xyz:false" result.page

let tests =
  "application" >:::
  [ test_login_login; test_admin_set_pass; test_admin_set_pass_fail;
    test_forgot_main ]

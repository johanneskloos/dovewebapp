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

type init = {
  db_setup:string;
  mail_forgot: string;
  mail_new: string;
  page_login: string;
  page_admin_user: string;
  page_admin_admin: string;
  page_forgot: string
}

let format_message_success =
  "success:" ^
  "{% if msg.key == \"upd_password\" %}upd_password:{{msg.user}}" ^
  "{% elseif msg.key == \"upd_email\" %}upd_email:{{msg.user}}:" ^
  "{% if (msg.mail is defined) %}{{msg.mail}}{% else %}(none){% endif %}" ^
  "{% elseif msg.key == \"set_user\" %}set_user:{{msg.user}}" ^
  "{% elseif msg.key == \"set_admin\" %}set_admin:{{msg.user}}" ^
  "{% elseif msg.key == \"token_sent\" %}token_sent:{{msg.user}}" ^
  "{% elseif msg.key == \"token_deleted\" %}token_deleted:{{msg.user}}" ^
  "{% elseif msg.key == \"created\" %}created:{{msg.user}}:{{msg.level}}" ^
  "{% if (msg.mail is defined)  %}mailed={{msg.mail}}" ^
  "{% elseif (msg.token is defined) %}token={{msg.token}}" ^
  "{% else %}finished{%endif%}" ^
  "{% elseif msg.key == \"user_deleted\" %}user_deleted:{{msg.user}}"^
  "{% else %}unknown:{{msg.key}}{%endif%}"
let format_message_fail =
  "failure:" ^
  "{% if msg.key == \"err_db\" %}err_db:{{msg.detail}}" ^
  "{% elseif msg.key == \"err_ext\" %}err_ext:{{msg.detail}}" ^
  "{% elseif msg.key == \"err_delete_all_admin\" %}err_delete_all_admin" ^
  "{% elseif msg.key == \"err_delete_logged_in\" %}err_delete_logged_in" ^
  "{% elseif msg.key == \"err_delete_unconfirmed\" %}" ^
  "err_delete_unconfirmed: {{msg.user}}" ^
  "{% elseif msg.key == \"err_pw_mismatch\" %}err_pw_mismatch" ^
  "{% elseif msg.key == \"err_auth_user\" %}err_auth_user" ^
  "{% elseif msg.key == \"err_auth_admin\" %}err_auth_admin" ^
  "{% else %}unknown:{{msg.key}}{%endif%}"
let format_messages =
  "{% for msg in infos%}\n" ^ format_message_success ^ "\n{%endfor%}" (* ^
  "{% for msg in errors%}\n" ^ format_message_fail ^ "\n{%endfor%}"*)
let format_admin_user =
  "user:{{user}}\n" ^
  "alt_email:{% if (alt_email is defined) %}" ^
  "{{alt_email}}{% else %}(None){% endif %}\n" ^
  format_messages
let format_user =
  "user:{{user.user}}:{{user.level}}:" ^
  "{% if user.email is defined %}" ^
  "{{user.email}}{%else%}(none){%endif%}:" ^
  "{% if user.token is defined %}" ^
  "{{user.token}}@{{user.expires}}" ^
  "{% elseif user.message is defined %}" ^
  "{{user.message}}" ^
  "{% else %}(no token){%endif%}\n"
let format_admin_admin =
  format_admin_user (*^
  "{% for user in users %}\n" ^ format_user ^ "\n{%endfor%}"*)
let db_setup =
  "CREATE TABLE users (username VARCHAR(30) PRIMARY KEY," ^
  "password VARCHAR(100), token CHAR(32), token_expires DATE, " ^
  "alternative_email VARCHAR(100), admin TINYINT NOT NULL);" ^
  "CREATE TABLE sessions (sessionid CHAR(32) PRIMARY KEY," ^
  "session_expires DATE NOT NULL, username VARCHAR(30) NOT NULL);" ^
  "INSERT INTO users VALUES (\"root\", \"t00r\", NULL, NULL, NULL, 1);" ^
  "INSERT INTO users VALUES (\"reader\", NULL, \"xyz\", 100, NULL, 1);" ^
  "INSERT INTO sessions VALUES (\"sid1\", 100, \"root\")"

let default_init_data = {
  db_setup;
  mail_forgot = "{{token}}";
  mail_new = "{{token}}";
  page_login =
    "{% if (message is undefined) %}(undefined)" ^
    "{% elseif message.key == \"login_failed\" %}login_failed" ^
    "{% elseif message.key == \"token_sent\" %}token_sent:{{message.user}}" ^
    "{% else %}Weird message: {{message.key}}{% endif %}";
  page_admin_user = format_admin_user;
  page_admin_admin = format_admin_admin;
  page_forgot = "{{user}}:{{token}}:{{pw_mismatch}}"
}

type output = {
  mails: mail list;
  page: string
}

let run ctxt ?path ?(initdata=default_init_data) ?cookies query =
  bracket_db_lock ctxt;
  let dir = bracket_tmpdir ctxt in
  Config.(set_command_line datadir) dir;
  Config.(set_command_line database) (Filename.concat dir "users.db");
  let db = Sqlite3.db_open Config.(get database) in
  Database.expect_ok (Sqlite3.exec db initdata.db_setup);
  ignore (Sqlite3.db_close db);
  let write_file file content =
    let chan = open_out (Filename.concat dir file) in
    output_string chan content;
    close_out chan in
  write_file "forgot.822" initdata.mail_forgot;
  write_file "new.822" initdata.mail_new;
  write_file "login.html" initdata.page_login;
  write_file "admin_user.html" initdata.page_admin_user;
  write_file "admin_admin.html" initdata.page_admin_admin;
  write_file "forgot.html" initdata.page_forgot;
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
    assert_equal ~pp_diff:(vs @@ Fmt.string)
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
           (Int64.of_int Config.(get sessions_timeout))
           (Database.get_int64 stmt 0);
         assert_equal ~pp_diff:(vs @@ Fmt.string)
           "root"
           (Database.get_str stmt 1));
    Application.drop_handle db;
    assert_page "user:root\nalt_email:(None)\n" result.page

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
      "user:root\nalt_email:(None)\n\nsuccess:upd_password:root\n"
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
    assert_page "user:root\nalt_email:(None)\n" result.page

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

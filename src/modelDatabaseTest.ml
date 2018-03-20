open OUnit2

let schema =
  {|
CREATE TABLE users (
	username VARCHAR(30) PRIMARY KEY,
	password VARCHAR(100),
	token CHAR(32),
	token_expires DATE,
	alternative_email VARCHAR(100),
	admin TINYINT NOT NULL
);

CREATE TABLE sessions (
sessionid CHAR(32) PRIMARY KEY,
session_expires DATE NOT NULL,
username VARCHAR(30) NOT NULL)
  |}
module ExternalsMock = struct
  let auth ~user ~pass = user = "foo" && pass = "bar"
  let password_encode ~user ~pass =
    if pass = "bad" then Error "Bad password" else Ok ("hash:" ^ pass)
  let token_counter = ref 0
  let generate_token () =
    incr token_counter;
    "tok" ^ string_of_int !token_counter
  let timestamp = Int64.of_int
end

let assert_equal ?ctxt ?cmp ?pp_diff ?msg ?(fmt=Fmt.nop) =
  assert_equal ?ctxt ?cmp ?pp_diff ?msg ~printer:(Fmt.to_to_string fmt)
let assert_raises_some ?msg fn =
  try
    fn ();
    assert_failure
      ((match msg with Some prefix -> prefix ^ ": " | None -> "") ^
       "Expected an exception")
  with _ -> ()

let make_database_test ~title fn =
  let open Database in
  title >:: fun ctx ->
    ExternalsMock.token_counter := 0;
    let dir = bracket_tmpdir ctx in
    let db = Sqlite3.db_open (Filename.concat dir "test.db") in
    Database.expect_ok (Sqlite3.exec db schema);
    fn { handle = db; in_transaction = false }

module M = ModelDatabase.Make(ExternalsMock)

let count_users db =
  Database.execute_select_one db "SELECT count(*) FROM users" []
    (fun row -> Database.get_int64 row 0) |> Int64.to_int

let count_sessions db =
  Database.execute_select_one db "SELECT count(*) FROM sessions" []
    (fun row -> Database.get_int64 row 0) |> Int64.to_int

let encode_expiry = function
  | Some time -> Sqlite3.Data.INT time
  | None -> Sqlite3.Data.NULL

let pp_level pp = function
  | Model.Admin -> Fmt.string pp "admin"
  | Model.User -> Fmt.string pp "user"
let int64_max_diff delta x y = Int64.(abs (sub x y) < delta)
let option_cmp some_cmp x y =
  match x, y with
  | Some x, Some y -> some_cmp x y
  | None, None -> true
  | _, _ -> false

let assert_has_user db ~user ~pass ~email ~token ~token_expires ~level =
  try
    Database.execute_select_one db
      ("SELECT password, token, token_expires, alternative_email, " ^
       "admin FROM users WHERE username = ?")
      Database.[str user]
      (fun row ->
         let open Database in
         let pass' = get_stropt row 0
         and token' = get_stropt row 1
         and token_expires' = get_int64opt row 2
         and email' = get_stropt row 3
         and level' =
           if get_bool row 4 then Model.Admin else Model.User in
         assert_equal ~msg:"Passwords" ~fmt:Fmt.(option string)
           pass pass';
         assert_equal ~msg:"Tokens" ~fmt:Fmt.(option string)
           token token';
         assert_equal ~msg:"Token expiry"
           ~cmp:(option_cmp (int64_max_diff 10L))
           ~fmt:Fmt.(option int64) token_expires token_expires';
         assert_equal ~msg:"E-Mail" ~fmt:Fmt.(option string)
           email email';
         assert_equal ~msg:"Level" ~fmt:pp_level level level')
  with Database.NotEnoughResults ->
    assert_failure ("User " ^ user ^ " not in database")

let add_user db ~user ~pass ~email ~token ~token_expires ~level =
  Database.execute_update db
    ("INSERT INTO users (username, password, token, token_expires, " ^
     "alternative_email, admin) VALUES (?, ?, ?, ?, ?, ?)")
    Database.[str user; stropt pass; stropt token;
              encode_expiry token_expires; stropt email;
              bool (match level with Model.User -> false | _ -> true)]

let has_session db ~user ~session ~expires =
  Database.execute_select_one db
    ("SELECT count(*) FROM sessions WHERE username = ? AND " ^
     "sessionid = ? AND abs (session_expires - ?) < 10")
    Database.[str user; stropt session; encode_expiry expires]
    (fun row -> Database.get_int64 row 0 = 1L)

let test_session_login_success =
  make_database_test ~title:"session_login, good case"
    (fun db ->
       assert_equal ~fmt:Fmt.(option string) (Some "tok1")
         (M.session_login db ~user:"foo" ~pass:"bar");
       assert_equal ~fmt:Fmt.int 1 (count_sessions db);
       assert_bool "No sessoin for foo"
         (has_session db "foo" (Some "tok1")
            (Some (Int64.of_int (Config.(get sessions_timeout))))))

let test_session_login_fail =
  make_database_test ~title:"session_login, bad case"
    (fun db ->
       assert_equal ~fmt:Fmt.(option string) None
         (M.session_login db ~user:"foo" ~pass:"baz");
       assert_equal ~fmt:Fmt.int 0 (count_sessions db))

let setup_session db timeout =
  Database.execute_update db
    ("INSERT INTO sessions (username, sessionid, session_expires) " ^
     "VALUES ('foo', 'sid1', ?)")
    [Sqlite3.Data.INT timeout]

let test_session_logout =
  make_database_test ~title:"session_logout"
    (fun db ->
       setup_session db 100L;
       M.session_logout db { auth_session = Some "sid1";
                             auth_user = "foo";
                             auth_level = Model.User };
       assert_equal ~fmt:Fmt.int 0 (count_sessions db))

let test_session_logout_diff_sid =
  make_database_test ~title:"session_logout, different sid"
    (fun db ->
       setup_session db 100L;
       M.session_logout db { auth_session = Some "sid2";
                             auth_user = "foo";
                             auth_level = Model.User };
       assert_equal ~fmt:Fmt.int 1 (count_sessions db);
       assert_bool "Has no session sid1"
         (has_session db "foo" (Some "sid1")
            (Some 100L)))

let test_session_logout_no_sid =
  make_database_test ~title:"session_logout, no sid"
    (fun db ->
       setup_session db 100L;
       M.session_logout db { auth_session = None;
                             auth_user = "foo";
                             auth_level = Model.User };
       assert_equal ~fmt:Fmt.int 1 (count_sessions db);
       assert_bool "Has no session sid1"
         (has_session db "foo" (Some "sid1")
            (Some 100L)))

let pp_level pp = function
  | Model.User -> Format.fprintf pp "user"
  | Model.Admin -> Format.fprintf pp "admin"
let pp_authdata pp Model.{ auth_session; auth_user; auth_level } =
  let open Fmt in
  Format.fprintf pp "%a: %s, %a" (option string) auth_session auth_user
    pp_level auth_level

let test_session_retrieve_impl db =
  setup_session db Int64.max_int;
  add_user ~user:"foo" ~pass:None ~email:None ~token:None
    ~token_expires:None ~level:Model.User db;
  assert_equal ~fmt:(Fmt.option pp_authdata)
    (Some Model.{ auth_session = Some "sid1"; auth_user = "foo";
                  auth_level = User })
    (M.session_retrieve db "sid1")
let test_session_retrieve =
  make_database_test ~title:"session_retrieve, success case"
    test_session_retrieve_impl


let test_session_retrieve_2 =
  make_database_test ~title:"session_retrieve, success case (2)"
    (fun db ->
       setup_session db Int64.max_int;
       add_user ~user:"foo" ~pass:None ~email:None ~token:None
         ~token_expires:None ~level:Model.Admin db;
       assert_equal ~fmt:(Fmt.option pp_authdata)
         (Some Model.{ auth_session = Some "sid1"; auth_user = "foo";
                       auth_level = Admin })
         (M.session_retrieve db "sid1"))

let test_session_no_session =
  make_database_test ~title:"session_retrieve, no session"
    (fun db ->
       setup_session db Int64.max_int;
       add_user ~user:"foo" ~pass:None ~email:None ~token:None
         ~token_expires:None ~level:Model.Admin db;
       assert_equal ~fmt:(Fmt.option pp_authdata)
         None (M.session_retrieve db "sid2"))

let test_session_no_user =
  make_database_test ~title:"session_retrieve, no user data"
    (fun db ->
       setup_session db Int64.max_int;
       assert_equal ~fmt:(Fmt.option pp_authdata)
         None (M.session_retrieve db "sid1"))

let test_session_from_token =
  make_database_test ~title:"session_from_token"
    (fun db ->
       add_user ~user:"foo" ~pass:None ~email:None ~token:(Some "tok")
         ~token_expires:(Some Int64.max_int) ~level:Model.Admin db;
       assert_equal ~fmt:(Fmt.option pp_authdata)
         (Some Model.{ auth_session = None; auth_user = "foo";
                       auth_level = User })
         (M.session_from_token db ~user:"foo" ~token:"tok"))

let test_session_from_token_expired =
  make_database_test ~title:"session_from_token, expired token"
    (fun db ->
       add_user ~user:"foo" ~pass:None ~email:None ~token:(Some "tok")
         ~token_expires:(Some Int64.min_int) ~level:Model.Admin db;
       assert_equal ~fmt:(Fmt.option pp_authdata) None
         (M.session_from_token db ~user:"foo" ~token:"tok"))

let test_session_from_token_wrong_token =
  make_database_test ~title:"session_from_token, wrong token"
    (fun db ->
       add_user ~user:"foo" ~pass:None ~email:None ~token:(Some "tok")
         ~token_expires:(Some Int64.max_int) ~level:Model.Admin db;
       assert_equal ~fmt:(Fmt.option pp_authdata) None
         (M.session_from_token db ~user:"foo" ~token:"wrong"))

let test_session_from_token_wrong_user =
  make_database_test ~title:"session_from_token, wrong user"
    (fun db ->
       add_user ~user:"foo" ~pass:None ~email:None ~token:(Some "tok")
         ~token_expires:(Some Int64.max_int) ~level:Model.Admin db;
       assert_equal ~fmt:(Fmt.option pp_authdata) None
         (M.session_from_token db ~user:"bar" ~token:"tok"))

let test_session_from_token_no_token =
  make_database_test ~title:"session_from_token, no token"
    (fun db ->
       add_user ~user:"foo" ~pass:None ~email:None ~token:None
         ~token_expires:None ~level:Model.Admin db;
       assert_equal ~fmt:(Fmt.option pp_authdata) None
         (M.session_from_token db ~user:"foo" ~token:"wrong"))

let auth_user =
  Model.{ auth_user = "foo"; auth_session = Some "sid";
          auth_level = User }

let auth_admin =
  Model.{ auth_user = "foo"; auth_session = Some "sid";
          auth_level = Admin }

let test_user_update_password_user =
  make_database_test ~title:"user_update_password, as user"
    (fun db ->
       let user = "foo"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       M.user_update_password db auth_user ~user ~pass:"baz";
       assert_has_user db ~user ~pass:(Some "hash:baz")
         ~email ~token ~token_expires ~level)

let test_user_update_password_user_reset_token =
  make_database_test
    ~title:"user_update_password, as user, resetting token"
    (fun db ->
       let user = "foo"
       and email = None
       and token = Some "tok"
       and token_expires = Some Int64.max_int
       and level = Model.User in
       add_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       M.user_update_password db auth_user ~user ~pass:"baz";
       assert_has_user db ~user ~pass:(Some "hash:baz")
         ~email ~token:None ~token_expires:None ~level)


let test_user_update_password_admin =
  make_database_test ~title:"user_update_password, as admin"
    (fun db ->
       let user = "foo"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       M.user_update_password db auth_admin ~user ~pass:"baz";
       assert_has_user db ~user ~pass:(Some "hash:baz")
         ~email ~token ~token_expires ~level)

let test_user_update_password_nonexistant_user =
  make_database_test
    ~title:"user_update_password, user does not exist"
    (fun db ->
       let user = "foo"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user:"blah" ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       M.user_update_password db auth_admin ~user ~pass:"baz";
       assert_has_user db ~user:"blah" ~pass:(Some "bar")
         ~email ~token ~token_expires ~level)

let test_user_update_password_wrong_user =
  make_database_test
    ~title:"user_update_password, wrong user and not admin"
    (fun db ->
       let email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user:"blah" ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       assert_raises_some
         (fun () -> M.user_update_password db auth_user
             ~user:"blah" ~pass:"baz"))

let test_user_update_password_bad_password =
  make_database_test ~title:"user_update_password, bad password"
    (fun db ->
       let user = "foo"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user:"blah" ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       assert_raises_some
         (fun () ->
            M.user_update_password db auth_admin ~user ~pass:"bad"))

let test_user_update_alternative_email_user =
  make_database_test ~title:"user_update_alternative_email, as user"
    (fun db ->
       let user = "foo"
       and pass = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email:(Some "xyz@example.com")
         ~token ~token_expires ~level;
       M.user_update_alternative_email db auth_user
         ~user ~mail:(Some "abc@example.net");
       assert_has_user db ~user ~pass
         ~email:(Some "abc@example.net") ~token ~token_expires ~level)

let test_user_update_alternative_email_user_from_none =
  make_database_test
    ~title:"user_update_alternative_email, as user, from unset"
    (fun db ->
       let user = "foo"
       and pass = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email:None
         ~token ~token_expires ~level;
       M.user_update_alternative_email db auth_user
         ~user ~mail:(Some "abc@example.net");
       assert_has_user db ~user ~pass
         ~email:(Some "abc@example.net") ~token ~token_expires ~level)

let test_user_update_alternative_email_user_to_unset =
  make_database_test
    ~title:"user_update_alternative_email, as user, to unset"
    (fun db ->
       let user = "foo"
       and pass = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email:(Some "xyz@example.com")
         ~token ~token_expires ~level;
       M.user_update_alternative_email db auth_user ~user ~mail:None;
       assert_has_user db ~user ~pass
         ~email:None ~token ~token_expires ~level)

let test_user_update_alternative_email_nonexistant_user =
  make_database_test
    ~title:"user_update_alternative_email, user does not exist"
    (fun db ->
       let user = "foo"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       M.user_update_alternative_email db auth_admin
         ~user:"blah" ~mail:(Some "a@b.io");
       assert_has_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level)

let test_user_update_alternative_email_wrong_user =
  make_database_test
    ~title:"user_update_alternative_email, wrong user and not admin"
    (fun db ->
       let email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user:"blah" ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       assert_raises_some
         (fun () -> M.user_update_alternative_email db auth_user
             ~user:"blah" ~mail:(Some "a@b.io")))

let test_user_delete =
  make_database_test ~title:"user_delete"
    (fun db ->
       let user = "foo"
       and email = None
       and pass = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       M.user_delete db auth_user user;
       assert_equal ~fmt:Fmt.int 0 (count_users db))

let test_user_delete_admin =
  make_database_test ~title:"user_delete, as admin"
    (fun db ->
       let user = "bar"
       and email = None
       and pass = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       M.user_delete db auth_admin user;
       assert_equal ~fmt:Fmt.int 0 (count_users db))

let test_user_delete_wrong_user =
  make_database_test ~title:"user_delete, as wrong user"
    (fun db ->
       let user = "bar"
       and email = None
       and pass = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       assert_raises_some (fun () -> M.user_delete db auth_user user))

let test_user_create_token =
  make_database_test ~title:"user_create_token"
    (fun db ->
       let user = "bar"
       and email = None
       and pass = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       assert_equal ~fmt:Fmt.string
         "tok1" (M.user_create_token db user);
       assert_has_user db ~user ~pass ~email ~token:(Some "tok1") ~level
         ~token_expires:(Some (Int64.of_int Config.(get token_lifetime))
                        ))

let test_user_create_token_expired =
  make_database_test ~title:"user_create_token, expired token"
    (fun db ->
       let user = "bar"
       and email = None
       and pass = None
       and token = Some "exptok"
       and token_expires = Some Int64.min_int
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       assert_equal ~fmt:Fmt.string
         "tok1" (M.user_create_token db user);
       assert_has_user db ~user ~pass ~email ~level ~token:(Some "tok1")
         ~token_expires:(Some (Int64.of_int Config.(get token_lifetime))
                        ))

let test_user_create_token_reuse =
  make_database_test ~title:"user_create_token, recycled token"
    (fun db ->
       let user = "bar"
       and email = None
       and pass = None
       and token = Some "rtok"
       and token_expires = Some Int64.max_int
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       assert_equal ~fmt:Fmt.string
         "rtok" (M.user_create_token db user);
       assert_has_user db ~user ~pass ~email ~level
         ~token:(Some "rtok") ~token_expires:(Some Int64.max_int))

let test_user_update_admin_user =
  make_database_test ~title:"user_update_admin, as user"
    (fun db ->
       let user = "foo"
       and pass = None
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       assert_raises_some (fun () ->
           M.user_update_admin db auth_user ~user ~level:Model.User))

let test_user_update_admin_admin =
  make_database_test ~title:"user_update_admin, as admin"
    (fun db ->
       let user = "foo"
       and pass = None
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass ~email ~token ~token_expires ~level;
       M.user_update_admin db auth_admin ~user ~level:Model.Admin;
       assert_has_user db ~user ~pass ~email ~token ~token_expires
         ~level:Model.Admin)

let test_user_update_admin_nonexistant_user =
  make_database_test ~title:"user_update_admin, user does not exist"
    (fun db ->
       let user = "foo"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       M.user_update_admin db auth_admin
         ~user:"blah" ~level:Model.Admin;
       assert_has_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level)

let test_user_update_admin_wrong_user =
  make_database_test
    ~title:"user_update_admin, wrong user and not admin"
    (fun db ->
       let email = None
       and token = None
       and token_expires = None
       and level = Model.User in
       add_user db ~user:"blah" ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       assert_raises_some
         (fun () -> M.user_update_admin db auth_user
             ~user:"blah" ~level:Model.User))

let test_user_delete_token_user =
  make_database_test ~title:"user_delete_token, as user"
    (fun db ->
       let user = "foo"
       and email = None
       and token = Some "tok"
       and token_expires = Some Int64.max_int
       and level = Model.User in
       add_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       assert_raises_some
         (fun () -> M.user_delete_token db auth_user user))

let test_user_delete_token_admin =
  make_database_test ~title:"user_delete_token, as admin"
    (fun db ->
       let user = "foo"
       and email = None
       and token = Some "tok"
       and token_expires = Some Int64.max_int
       and level = Model.User in
       add_user db ~user ~pass:(Some "bar")
         ~email ~token ~token_expires ~level;
       M.user_delete_token db auth_admin user;
       assert_has_user db ~user ~pass:(Some "bar")
         ~email ~token:None ~token_expires:None ~level)

let test_user_create_nopw_user =
  make_database_test ~title:"user_create_nopw, as user"
    (fun db ->
       assert_raises_some
         (fun () ->
            M.user_create_nopw db auth_user ~user:"blah"
              ~altemail:None ~level:Model.User))

let test_user_create_nopw_admin =
  make_database_test ~title:"user_create_nopw, as admin"
    (fun db ->
       let user = "blah"
       and email = None
       and level = Model.User in
       let token = M.user_create_nopw db auth_admin ~user
           ~altemail:email ~level in
       assert_equal ~fmt:Fmt.string "tok1" token;
       assert_has_user db ~user ~pass:None ~token:(Some token)
         ~level ~email
         ~token_expires:(Some (Int64.of_int Config.(get token_lifetime))
                        ))

let test_user_create_nopw_admin_dup =
  make_database_test ~title:"user_create_nopw, as admin, user exists"
    (fun db ->
       let user = "blah"
       and email = None
       and level = Model.User in
       add_user db ~user ~pass:None ~token:None
         ~token_expires:None ~level ~email;
       assert_raises_some (fun () ->
           M.user_create_nopw db auth_admin
             ~user ~altemail:email ~level))

let test_user_create_pw_user =
  make_database_test ~title:"user_create_pw, as user"
    (fun db ->
       assert_raises_some
         (fun () ->
            M.user_create_pw db auth_user ~user:"blah" ~pass:"xyz"
              ~altemail:None ~level:Model.User))

let test_user_create_pw_admin =
  make_database_test ~title:"user_create_pw, as admin"
    (fun db ->
       let user = "blah"
       and email = None
       and level = Model.User
       and pass = "xyz" in
       M.user_create_pw db auth_admin ~user ~altemail:email
         ~level ~pass;
       assert_has_user db ~user ~pass:(Some ("hash:xyz")) ~token:None
         ~token_expires:None ~level ~email)

let test_user_create_pw_admin_dup =
  make_database_test ~title:"user_create_pw, as admin, user exists"
    (fun db ->
       let user = "blah"
       and email = None
       and pass = "xyz"
       and level = Model.User in
       add_user db ~user ~pass:None ~token:None
         ~token_expires:None ~level ~email;
       assert_raises_some (fun () ->
           M.user_create_pw db auth_admin ~user ~altemail:email
             ~level ~pass))

let test_user_create_pw_admin_bad_pass =
  make_database_test ~title:"user_create_pw, as admin, bad password"
    (fun db ->
       let user = "blah"
       and email = None
       and pass = "bad"
       and level = Model.User in
       assert_raises_some (fun () ->
           M.user_create_pw db auth_admin ~user ~altemail:email
             ~level ~pass))

let test_user_list_user =
  make_database_test ~title:"user_list, as user"
    (fun db -> assert_raises_some (fun () -> M.user_list db auth_user))

let test_user_list_admin =
  make_database_test ~title:"user_list, as admin"
    (fun db ->
       let user = "foo"
       and pass = Some "xyz"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.Admin in
       add_user db ~user ~pass ~token ~token_expires ~level ~email;
       assert_equal
         [Model.{ user_name = user; user_token = token;
                  user_expires = token_expires;
                  user_alt_email = email; user_level = level }]
         (M.user_list db auth_admin))

let test_user_get_email =
  make_database_test ~title:"user_get_email"
    (fun db ->
       let user = "foo"
       and pass = Some "xyz"
       and email = Some "x@y.tk"
       and token = None
       and token_expires = None
       and level = Model.Admin in
       add_user db ~user ~pass ~token ~token_expires ~level ~email;
       assert_equal ~fmt:Fmt.(option string) email
         (M.user_get_email db user))

let test_user_get_email_none =
  make_database_test ~title:"user_get_email, no email set"
    (fun db ->
       let user = "foo"
       and pass = Some "xyz"
       and email = None
       and token = None
       and token_expires = None
       and level = Model.Admin in
       add_user db ~user ~pass ~token ~token_expires ~level ~email;
       assert_equal ~fmt:Fmt.(option string) email
         (M.user_get_email db user))

let test_user_get_email_no_such_user =
  make_database_test ~title:"user_get_email, no such user"
    (fun db ->
       assert_equal ~fmt:Fmt.(option string)
         None (M.user_get_email db "foo"))

let tests =
  "ModelDatabase" >:::
  [ test_session_login_success; test_session_login_fail;
    test_session_logout; test_session_logout_diff_sid;
    test_session_logout_no_sid; test_session_retrieve;
    test_session_retrieve_2; test_session_no_session;
    test_session_no_user; test_session_from_token;
    test_session_from_token_expired;
    test_session_from_token_wrong_token;
    test_session_from_token_wrong_user;
    test_session_from_token_no_token;
    test_user_update_password_user;
    test_user_update_password_user_reset_token;
    test_user_update_password_admin;
    test_user_update_password_nonexistant_user;
    test_user_update_password_wrong_user;
    test_user_update_password_bad_password;
    test_user_update_alternative_email_user;
    test_user_update_alternative_email_user_from_none;
    test_user_update_alternative_email_user_to_unset;
    test_user_update_alternative_email_nonexistant_user;
    test_user_update_alternative_email_wrong_user;
    test_user_delete; test_user_delete_admin;
    test_user_delete_wrong_user; test_user_create_token;
    test_user_create_token_expired;
    test_user_create_token_reuse;
    test_user_update_admin_user;
    test_user_update_admin_admin;
    test_user_update_admin_nonexistant_user;
    test_user_update_admin_wrong_user;
    test_user_delete_token_user;
    test_user_delete_token_admin;
    test_user_create_nopw_user;
    test_user_create_nopw_admin;
    test_user_create_nopw_admin_dup;
    test_user_create_pw_user; test_user_create_pw_admin;
    test_user_create_pw_admin_dup;
    test_user_create_pw_admin_bad_pass;
    test_user_list_user; test_user_list_admin;
    test_user_get_email; test_user_get_email_none;
    test_user_get_email_no_such_user ]

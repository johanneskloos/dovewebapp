open Model
type user_data = {
  password: string option;
  token: (string * int64) option;
  alternative_email: string option;
  admin: bool
} [@@deriving show]

type session = {
  expires: int64;
  username: string
} [@@deriving show]

type database = {
  mutable db_users: user_data StringMap.t;
  mutable db_sessions: session StringMap.t;
  key: string
} [@@deriving show]

type db = database

let current_time = ref 0L

let find_fresh_key map =
  let rec probe i =
    let key = Format.sprintf "#%d" i in
    if StringMap.mem key map then probe (i+1) else key
  in probe 0

let session_login db ~user ~pass =
  try
    let { password } = StringMap.find user db.db_users in
    if (Some pass <> password) then None
    else
      let session_id = find_fresh_key db.db_sessions in
      let session_data =
        { username = user; expires = Int64.add (!current_time) 300L } in
      db.db_sessions <-
        StringMap.add session_id session_data db.db_sessions;
      Some session_id
  with Not_found -> None

let session_logout db = function
  | { auth_session = Some key } ->
    db.db_sessions <- StringMap.remove key db.db_sessions
  | _ -> ()

let session_retrieve db token =
  try
    let { expires; username } = StringMap.find token db.db_sessions in
    if expires < !current_time then None
    else
      let { admin } = StringMap.find username db.db_users in
      Some { auth_session = Some token; auth_user = username;
             auth_level = if admin then Admin else User }
  with Not_found -> None

let session_from_token db ~user ~token =
  match StringMap.find user db.db_users with
  | { admin; token = Some (token2, expires) }
    when token = token2 && expires >= !current_time ->
    Some { auth_session = None; auth_user = user;
           auth_level = if admin then Admin else User }
  | _ -> None
  | exception Not_found -> None

let update_user_if_exists db user fn =
  try
    let users = db.db_users in
    db.db_users <-
      StringMap.add user (fn (StringMap.find user users)) users
  with Not_found -> ()

let user_update_password db session ~user ~pass =
  need_same_user session user;
  update_user_if_exists db user
    (fun data -> { data with password = Some pass })

let user_update_alternative_email db session ~user ~mail =
  need_same_user session user;
  update_user_if_exists db user
    (fun data -> { data with alternative_email = mail })

let user_delete db session user =
  need_same_user session user;
  db.db_users <- StringMap.remove user db.db_users

let mktoken user =
  (Digest.(to_hex (string (user ^ Int64.to_string !current_time))),
   Int64.add !current_time 500L)

let user_create_token db user =
  let token = mktoken user in
  update_user_if_exists db user
    (fun data -> { data with token = Some token });
  fst token

let user_update_admin db session ~user ~level =
  need_admin session;
  update_user_if_exists db user
    (fun data ->
       { data with
         admin = match level with Admin -> true | User -> false })

let user_delete_token db session user =
  need_admin session;
  update_user_if_exists db user (fun data -> { data with token = None })

let user_create_nopw db session ~user ~altemail ~level =
  need_admin session;
  if StringMap.mem user db.db_users then failwith "User exists";
  let token = mktoken user in
  let data =
    { password = None; token = Some token;
      alternative_email = altemail;
      admin = match level with Admin -> true | User -> false } in
  db.db_users <- StringMap.add user data db.db_users;
  fst token

let user_create_pw db session ~user ~pass ~altemail ~level =
  need_admin session;
  if StringMap.mem user db.db_users then failwith "User exists";
  let data =
    { password = Some pass; token = None;
      alternative_email = altemail;
      admin = match level with Admin -> true | User -> false } in
  db.db_users <- StringMap.add user data db.db_users

let user_list db session =
  need_admin session;
  List.map (fun (user, { token; alternative_email; admin }) ->
      { user_name = user; user_alt_email = alternative_email;
        user_token =
          (match token with Some (tok, _) -> Some tok | None -> None);
        user_expires =
          (match token with Some (_, exp) -> Some exp | None -> None);
        user_level = if admin then Admin else User })
    (StringMap.bindings db.db_users)

let expire _ = ()

let user_task_run db session tasks =
  need_admin session;
  List.fold_left (fun tokens ->
      function
      | TaskSetPassword { user; pass } ->
        user_update_password db session ~user ~pass;
        tokens
      | TaskSetEMail { user; mail } ->
        user_update_alternative_email db session ~user ~mail;
        tokens
      | TaskCreateToken user ->
        { user; token = user_create_token db user } :: tokens
      | TaskDeleteToken user ->
        user_delete_token db session user;
        tokens
      | TaskSetAdmin { user; level } ->
        user_update_admin db session user level;
        tokens
      | TaskDelete user ->
        user_delete db session user;
        tokens)
    [] tasks


let user_get_email db user =
  match (StringMap.find user db.db_users).alternative_email
  with
  | Some addr -> Address addr
  | None -> NoAddress
  | exception Not_found -> NoSuchUser

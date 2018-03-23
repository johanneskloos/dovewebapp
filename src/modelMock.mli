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

val user_add : string -> user_data -> database -> unit
val session_add : string -> session -> database -> unit
val user_rem : string -> database -> unit
val session_rem : string -> database -> unit

val current_time: int64 ref
include S with type db = database

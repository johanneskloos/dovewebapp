open Model
module StringMap: Map.S with type key = string
type user_data = {
  password: string option;
  token: (string * int64) option;
  alternative_email: string option;
  admin: bool
}
type session = {
  expires: int64;
  username: string
}
type database = {
  mutable db_users: user_data StringMap.t;
  mutable db_sessions: session StringMap.t
}
val current_time: int64 ref
include S with type db = database

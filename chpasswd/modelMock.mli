open Model
module StringMap: Map.S with type key = string
type user_data = {
  password: string option;
  token: (string * float) option;
  alternative_email: string option;
  admin: bool
}
type session = {
  expires: float;
  username: string
}
type database = {
  mutable db_users: user_data StringMap.t;
  mutable db_sessions: session StringMap.t
}
val current_time: float ref
include S with type db = database

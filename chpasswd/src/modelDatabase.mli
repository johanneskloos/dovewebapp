module type Externals = sig
  val auth: user:string -> pass:string -> bool
  val password_encode: user:string -> pass:string -> (string, string) result
  val generate_token: unit -> string
  val timestamp: int -> int64
end

module Make(E: Externals): Model.S with type db = Database.db

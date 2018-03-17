val cookie_name : string
val validate_string : (char -> bool) -> string -> bool
val valid_user_chars : char -> bool
val validate_user : string -> bool
val valid_pass_chars : char -> bool
val validate_pass : string -> bool
val validate_email : string -> bool
exception InvalidUser of string
exception InvalidAddress of string
exception InvalidPass
exception ArgumentMissing of string
exception ArgumentEmpty of string
val get_user : Netcgi.cgi -> string -> string
val get_nonempty_string_option : Netcgi.cgi -> string -> string option
val get_nonempty_string : Netcgi.cgi -> string -> string
val get_mail_option : Netcgi.cgi -> string -> string option
val get_pass : Netcgi.cgi -> string -> string
val get_admin : Netcgi.cgi -> string -> Model.level
module Make(ModelImpl: Model.S)
    (ViewImpl: View.S with type model = ModelImpl.db): sig
  val event_login_login :
    ModelImpl.db -> string -> string -> Netcgi.cgi -> Netcgi.Cookie.t list * string
  val event_login_forgot : ModelImpl.db -> string -> string
  val event_login : ModelImpl.db -> Netcgi.cgi -> Netcgi.Cookie.t list * string
  val event_admin_logout : ModelImpl.db -> Model.authdata -> string
  val event_admin_change_password :
    ModelImpl.db -> Model.authdata -> string -> string -> string
  val event_admin_change_password_forgot :
    ModelImpl.db -> string -> Model.authdata -> string -> string -> string
  val event_admin_change_email : ModelImpl.db -> Model.authdata -> string -> string
  val event_admin_delete : ModelImpl.db -> Model.authdata -> bool -> string
  val event_admin_create :
    ModelImpl.db ->
    Model.authdata ->
    string -> string -> string option -> Model.level -> string
  val event_admin_mass_update : 'a -> 'b -> Netcgi.cgi -> 'c
  val event_admin : ModelImpl.db -> Netcgi.cgi -> string
  val event_forgot : ModelImpl.db -> Netcgi.cgi -> string
  val db_cached : ModelImpl.db option ref
  val handle_request : (unit -> ModelImpl.db) -> Netcgi.cgi -> unit
end


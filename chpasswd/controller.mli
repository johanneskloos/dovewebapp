val cookie_name : string
val validate_user : string -> string
val validate_pass : string -> string
val validate_email : string -> string
exception InvalidUser of string
exception InvalidAddress of string
exception InvalidPass
exception ArgumentMissing of string
exception ArgumentEmpty of string
module Make(ModelImpl: Model.S)
    (ViewImpl: View.S with type model = ModelImpl.db): sig
  val event_login : ViewImpl.model -> ViewImpl.view -> unit
  val event_admin : ViewImpl.model -> ViewImpl.view -> unit
  val event_forgot : ViewImpl.model -> ViewImpl.view -> unit
end


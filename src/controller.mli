val validate_user : string -> string
(** [validate_user] is the identity function on valid user names,
    and throws an [InvalidUser] exception otherwise.

    Valid user names are RFC822 atoms. *)

val validate_pass : string -> string
(** [validate_user] is the identity function on valid passwords,
    and throws an [InvalidPass] exception otherwise.

    Valid passwords are strings that do not contain line
    breaks or NUL characters. *)

val validate_email : string -> string
(** [validate_user] is the identity function on valid e-mail addresses,
    and throws an [InvalidAddress] exception otherwise.

    Valid e-mail addresses are as specified in RFC2822. *)

exception InvalidUser of string
exception InvalidAddress of string
exception InvalidPass
exception ArgumentMissing of string
exception ArgumentEmpty of string

module Make(ModelImpl: Model.S)
    (ViewImpl: View.S with type model = ModelImpl.db)
    (MailImpl: Mails.Strategy): sig
  type controller = {
    mailer: MailImpl.t;
    db: ModelImpl.db
  }

  val event_login : controller -> ViewImpl.view -> unit
  val event_admin : controller -> ViewImpl.view -> unit
  val event_forgot : controller -> ViewImpl.view -> unit
end


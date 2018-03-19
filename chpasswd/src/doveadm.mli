(** Interface to doveadm. *)

(** Encode a password using "doveadm pw".

    This function takes a username and a password. In the normal
    case, it returns [Ok hash], where [hash] is a representation
    of the hashed password. If something fails, it returns [Error reason],
    where [reason] is a human-readable string describing the reason
    for failure.

    @param user The user name. User names containing line breaks or NUL
      characters are rejected.
    @param pass The password. Passwords containing line breaks or NUL
      characters are rejected. *)
val password_encode : user:string -> pass:string -> (string, string) result

(** Try to authenticate a user using "doveadm auth test".

    This function takes a username and a password. It uses doveadm to
    check if the given user can authenticate with the given password,
    and returns the verdict.

    @param user The user name. User names containing line breaks or NUL
      characters are rejected.
    @param pass The password. Passwords containing line breaks or NUL
      characters are rejected. *)
val auth : user:string -> pass:string -> bool

module Assertion = struct
  include Kaputt.Assertion

  let rec list_eq elem_eq l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | x1::l1, x2::l2 ->
      if elem_eq x1 x2 then list_eq elem_eq l1 l2 else false
    | _, _ -> false
  let list_prn elem_prn l =
    match l with 
    | [] -> "[]"
    | hd::tl ->
      let buf = Buffer.create 100 in
      Buffer.add_string buf "[ ";
      Buffer.add_string buf (elem_prn hd);
      let rec rest = function
	| [] ->
	  Buffer.add_string buf " ]";
	  Buffer.contents buf
	| hd::tl ->
	  Buffer.add_string buf ", ";
	  Buffer.add_string buf (elem_prn hd);
	  rest tl
      in rest tl

  let option_eq elem_eq o1 o2 =
    match o1, o2 with
    | Some o1, Some o2 -> elem_eq o1 o2
    | None, None -> true
    | _, _ -> false
  let option_prn elem_prn = function
    | None -> "(none)"
    | Some v -> elem_prn v

  let id x = x
  let prn_level = Model.(function User -> "user" | Admin -> "admin")
  let prn_string_opt = option_prn id
  let prn_authdata Model.{ auth_session; auth_user; auth_level } =
    Format.sprintf "Session %s for %s (%s)"
      (prn_string_opt auth_session) auth_user (prn_level auth_level)
  let prn_user_entry Model.{ user_name; user_token; user_expires;
			     user_alt_email; user_level } =
    Format.sprintf "User %s (%s): E-Mail=%s, Token=%s"
      user_name (prn_level user_level) (prn_string_opt user_alt_email)
      (match user_token, user_expires with
       | Some token, Some expires ->
	 Format.sprintf "%s until %s" token (Time.format_timeout expires)
       | None, None -> "(none)"
       | _, _ -> "(inconsistent)")
  let prn_token_info Model.{ user; token } =
    Format.sprintf "Token for %s: %s" user token
  let prn_task =
    let open Model in
    function
    | TaskSetPassword { user; pass } ->
      Format.sprintf "%s: Set password to '%s'" user pass
    | TaskSetEMail { user; mail } ->
      Format.sprintf "%s: Set e-mail to %s" user (prn_string_opt mail)
    | TaskCreateToken user -> Format.sprintf "Create token for %s" user
    | TaskDeleteToken user -> Format.sprintf "Delete token for %s" user
    | TaskSetAdmin { user; level } ->
      Format.sprintf "Make %s %s" user (prn_level level)
    | TaskDelete user -> Format.sprintf "Delete %s" user

  let make_equal_option eq prn = make_equal (option_eq eq) (option_prn prn)
  let equal_string_opt = make_equal_option (=) id
  let equal_int_opt = make_equal_option (=) string_of_int
  let equal_float_opt = make_equal_option (=) string_of_float
  let equal_level = make_equal (=) prn_level
  let equal_authdata = make_equal (=) prn_authdata
  let equal_user_entry = make_equal (=) prn_user_entry
  let equal_token_info = make_equal (=) prn_token_info
  let equal_task = make_equal (=) prn_task
  let equal_string_ignorecase =
    make_equal String.(fun s s' -> lowercase_ascii s = lowercase_ascii s') id
end


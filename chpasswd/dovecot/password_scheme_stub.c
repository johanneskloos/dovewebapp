#include <dovecot/config.h>
#include <dovecot/lib.h>
#include <dovecot/password-scheme.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdlib.h>

CAMLprim value caml_password_generate_encoded(
	value plaintext,
	value user,
	value scheme) {
    CAMLparam3(plaintext, user, scheme);
    CAMLlocal1(result);
    const char *hash;
    if (password_generate_encoded(String_val(plaintext),
		String_val(user), String_val(scheme), &hash)) {
	result = caml_alloc(1, 0);
	Store_field(result, 0, caml_copy_string(hash));
	free((void *)hash);
    } else {
	result = Int_val(0);
    }
    CAMLreturn(result);
}

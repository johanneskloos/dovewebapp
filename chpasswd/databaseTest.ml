open Database
open Kaputt

let make_sql_test ~title schema (fn: Database.db -> unit) =
  Test.make_assert_test ~title
    (fun () -> DatabaseTestTools.setup_database schema)
    (fun handle -> fn { handle = snd handle; in_transaction = false }; handle)
    DatabaseTestTools.teardown_database

let test_execute_update =
  make_sql_test ~title:"Test SQL statement updates, no arguments"
    "CREATE TABLE test (data SMALLINT)"
    (fun db ->
       Database.execute_update db "INSERT INTO test (data) VALUES (1)" [];
       let seen_data = ref [] in
       Assertion.equal Sqlite3.Rc.OK
	 (Sqlite3.exec_no_headers db.handle
	    ~cb:(function
		| [| Some value |] -> seen_data := value :: !seen_data
		| [| None |] -> invalid_arg ("null value, integer expected")
		| row -> invalid_arg ("Expected one column, got " ^
				      string_of_int (Array.length row)))
	    "SELECT data FROM test");
       Assertion.make_equal_list (=) (fun x -> x) ["1"] !seen_data)

let test_execute_update_fail =
  make_sql_test ~title:"Test failing update"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       Database.execute_update db "INSERT INTO test VALUES (1)" [];
       Assertion.raises (fun () ->
	   Database.execute_update db "INSERT INTO test VALUES (1)" []))

let assert_database_empty db =
  expect_ok (Sqlite3.exec_no_headers db.handle
	       ~cb:(fun _ -> failwith "No data expected")
	"SELECT data FROM test")

let test_execute_update_bind =
  make_sql_test ~title:"Tset SQL statement updates"
    "CREATE TABLE test (data SMALLINT)"
    (fun db ->
       Database.execute_update db "INSERT INTO test (data) VALUES (?)"
	 [Sqlite3.Data.INT 0L];
       Database.execute_update db "INSERT INTO test (data) VALUES (?)"
	 [Sqlite3.Data.INT 1L];
       let seen_data = ref [] in
       Assertion.equal Sqlite3.Rc.OK
	 (Sqlite3.exec_no_headers db.handle
	    ~cb:(function
		| [| Some value |] -> seen_data := value :: !seen_data
		| [| None |] -> invalid_arg ("null value, integer expected")
		| row -> invalid_arg ("Expected one column, got " ^
				      string_of_int (Array.length row)))
	    "SELECT data FROM test ORDER BY data");
       Assertion.make_equal_list (=) (fun x -> x) ["1"; "0"] !seen_data)

let test_execute_update_too_few_args =
  make_sql_test ~title:"Test update with too few args"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       Assertion.raises (fun () ->
	   Database.execute_update db "INSERT INTO test VALUES (?)" []);
       assert_database_empty db
    )

let test_execute_update_too_many_args =
  make_sql_test ~title:"Test update with too many args"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       let arg = Sqlite3.Data.INT 0L in
       Assertion.raises (fun () ->
	   Database.execute_update db "INSERT INTO test VALUES (?)" [arg; arg]);
       assert_database_empty db)

let setup_data_for_select { handle } =
  expect_ok (Sqlite3.exec handle "INSERT INTO test VALUES (0)");
  expect_ok (Sqlite3.exec handle "INSERT INTO test VALUES (1)");
  expect_ok (Sqlite3.exec handle "INSERT INTO test VALUES (2)")

let extract_int row =
  Assertion.equal_int 1 (Sqlite3.column_count row);
  match Sqlite3.column row 0 with
  | Sqlite3.Data.INT cell -> cell
  | c -> Assertion.fail_msg ("Expected cell of type INT, got " ^
			     Sqlite3.Data.to_string c)

let test_execute_select =
  make_sql_test ~title:"Test select"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.make_equal_list (=) Int64.to_string
	 [2L; 1L; 0L]
	 (execute_select db "SELECT data FROM test ORDER BY data" []
	    (fun row data -> extract_int row :: data)
	    []))

let test_execute_select_bind =
  make_sql_test ~title:"Test select with binding"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.make_equal_list (=) Int64.to_string
	 [2L; 1L]
	 (execute_select db "SELECT data FROM test WHERE data >= ? ORDER BY data"
	    [Sqlite3.Data.INT 1L]
	    (fun row data -> extract_int row :: data)
	    []))

let test_execute_select_too_few_args =
  make_sql_test ~title:"Test select with too few args"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       Assertion.raises (fun () ->
	   Database.execute_select db "SELECT data FROM test WHERE data >= ?" []
	     (fun _ () -> ()) ()))

let test_execute_select_too_many_args =
  make_sql_test ~title:"Test select with too many args"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       let arg = Sqlite3.Data.INT 0L in
       Assertion.raises (fun () ->
	   Database.execute_select db "SELECT data FROM test WHERE data >= ?"
	     [arg; arg]
	     (fun _ () -> ()) ()))

let test_execute_select_at_most_one_zero =
  make_sql_test ~title:"Test select (<= 1) with zero"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.equal None
	 (Database.execute_select_at_most_one db
	    "SELECT data FROM test WHERE data > ?"
	    [Sqlite3.Data.INT 2L] extract_int))

let test_execute_select_at_most_one_one =
  make_sql_test ~title:"Test select (<= 1) with one"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.equal (Some 2L)
	 (Database.execute_select_at_most_one db
	    "SELECT data FROM test WHERE data > ?"
	    [Sqlite3.Data.INT 1L] extract_int))

let test_execute_select_at_most_one_more =
  make_sql_test ~title:"Test select (<= 1) with one"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.raises (fun () ->
	   Database.execute_select_at_most_one db
	     "SELECT data FROM test WHERE data > ?"
	     [Sqlite3.Data.INT 0L] extract_int))

let test_execute_select_one_zero =
  make_sql_test ~title:"Test select (= 1) with zero"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.raises
	 (fun () ->
	    Database.execute_select_one db
	      "SELECT data FROM test WHERE data > ?"
	      [Sqlite3.Data.INT 2L] extract_int))

let test_execute_select_one_one =
  make_sql_test ~title:"Test select (= 1) with one"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.equal 2L
	 (Database.execute_select_one db
	    "SELECT data FROM test WHERE data > ?"
	    [Sqlite3.Data.INT 1L] extract_int))

let test_execute_select_one_more =
  make_sql_test ~title:"Test select (= 1) with one"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       setup_data_for_select db;
       Assertion.raises (fun () ->
	   Database.execute_select_one db
	     "SELECT data FROM test WHERE data > ?"
	     [Sqlite3.Data.INT 0L] extract_int))

let test_transaction_bracket_sucess =
  make_sql_test ~title:"Test transaction bracket - success case"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       transaction_bracket db setup_data_for_select;
       Assertion.make_equal_list (=) Int64.to_string
	 [2L; 1L; 0L]
	 (execute_select db "SELECT data FROM test ORDER BY data" []
	    (fun row data -> extract_int row :: data)
	    []))

let test_transaction_bracket_fail =
  make_sql_test ~title:"Test transaction bracket - failure case"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       Assertion.raises
	 (fun () -> transaction_bracket db
	     (fun db ->
		setup_data_for_select db;
		setup_data_for_select db));
       assert_database_empty db)

let test_transaction_bracket_fail_inner =
  make_sql_test ~title:"Test transaction bracket - inner failure case"
    "CREATE TABLE test (data SMALLINT PRIMARY KEY)"
    (fun db ->
       Assertion.raises
	 (fun () -> transaction_bracket db
	     (fun db ->
		setup_data_for_select db;
		transaction_bracket db setup_data_for_select));
       assert_database_empty db)

let tests =
  [test_execute_update; test_execute_update_fail; test_execute_update_bind;
   (*test_execute_update_too_few_args;*) test_execute_update_too_many_args;
   test_execute_select; test_execute_select_bind;
   (*test_execute_select_too_few_args;*) test_execute_select_too_many_args;
   test_execute_select_at_most_one_zero; test_execute_select_at_most_one_one;
   test_execute_select_at_most_one_more; test_execute_select_one_zero;
   test_execute_select_one_one; test_execute_select_one_more;
   test_transaction_bracket_sucess; test_transaction_bracket_fail;
   test_transaction_bracket_fail_inner]

let () = Test.run_tests tests
let setup_tmpdir () =
  let dir = Filename.temp_file "test" "db" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  dir

let delete_tmpdir dir = ignore (Sys.command ("rm -rf " ^ dir))

let setup_database setup =
  let dir = setup_tmpdir () in
  let db = Sqlite3.db_open (Filename.concat dir "test.db") in
  match Sqlite3.exec db setup with
  | Sqlite3.Rc.OK | Sqlite3.Rc.DONE -> (dir, db)
  | err -> failwith (Sqlite3.Rc.to_string err)

let teardown_database (dir, db) =
  while not (Sqlite3.db_close db) do () done;
  delete_tmpdir dir

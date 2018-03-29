(** Database interface *)
type db = { handle: Sqlite3.db; mutable in_transaction: bool }
exception DatabaseFailure of string
exception TypeError of string
exception NotEnoughResults
exception TooManyResults

let database_failure rc =
  raise (DatabaseFailure (Sqlite3.Rc.to_string rc))

let expect_ok = function
  | Sqlite3.Rc.OK -> ()
  | rc -> database_failure rc

let expect_done = function
  | Sqlite3.Rc.DONE -> ()
  | Sqlite3.Rc.ROW -> raise TooManyResults
  | rc -> database_failure rc

let expect_row = function
  | Sqlite3.Rc.ROW -> ()
  | Sqlite3.Rc.DONE -> raise NotEnoughResults
  | rc -> database_failure rc

let expect_row_or_done = function
  | Sqlite3.Rc.ROW -> true
  | Sqlite3.Rc.DONE -> false
  | rc -> database_failure rc

let prepare_cleanup db stmt cont =
  let open Sqlite3 in
  let stmt = prepare db.handle stmt in
  try
    let result = cont db stmt in
    expect_ok (finalize stmt); result
  with e ->
    ignore (finalize stmt);
    raise e

let prepare_bind_cleanup db stmt args cont =
  prepare_cleanup db stmt
    (fun db stmt ->
       List.iteri (fun i arg -> expect_ok (Sqlite3.bind stmt (i+1) arg))
         args;
       cont db stmt)

let execute_update db stmt args =
  prepare_bind_cleanup db stmt args
    (fun db stmt -> expect_done (Sqlite3.step stmt))

let execute_select db stmt args fn accu =
  let rec fold_db stmt accu =
    if expect_row_or_done (Sqlite3.step stmt) then
      fold_db stmt (fn stmt accu)
    else
      accu
  in prepare_bind_cleanup db stmt args
    (fun db stmt -> fold_db stmt accu)

let execute_select_at_most_one db stmt args fn =
  prepare_bind_cleanup db stmt args
    (fun _ stmt ->
       if expect_row_or_done (Sqlite3.step stmt) then
         let result = Some (fn stmt) in
         expect_done (Sqlite3.step stmt); result
       else
         None)

let execute_select_one db stmt args fn =
  prepare_bind_cleanup db stmt args
    (fun _ stmt ->
       expect_row (Sqlite3.step stmt);
       let result = fn stmt in
       expect_done (Sqlite3.step stmt); result)

let transaction_bracket db cont =
  if db.in_transaction then
    cont db
  else begin
    db.in_transaction <- true;
    execute_update db "BEGIN IMMEDIATE" [];
    try
      let result = cont db in
      execute_update db "COMMIT" [];
      db.in_transaction <- false;
      result
    with e ->
      begin try execute_update db "ROLLBACK" [] with _ -> () end;
      db.in_transaction <- false;
      raise e
  end

let connect path =
  { handle = 
      Sqlite3.db_open ~mode:`NO_CREATE ~mutex:`FULL ~cache:`PRIVATE path;
    in_transaction = false }

let str t = Sqlite3.Data.TEXT t
let stropt = function
  | Some t -> str t
  | None -> Sqlite3.Data.NULL
let int64 i = Sqlite3.Data.INT i
let bool b = Sqlite3.Data.INT (if b then 1L else 0L)
let int64opt = function
  | Some i -> int64 i
  | None -> Sqlite3.Data.NULL

let type_string = function
  | Sqlite3.Data.NONE -> "(none)"
  | Sqlite3.Data.NULL -> "NULL"
  | Sqlite3.Data.INT _ -> "an int"
  | Sqlite3.Data.FLOAT _ -> "a float"
  | Sqlite3.Data.TEXT _ -> "some text"
  | Sqlite3.Data.BLOB _ -> "a blob"
let type_error what i t =
  raise (TypeError (Format.sprintf "Expected %s at %d, but got %s"
                      what i (type_string t)))

let get_str stmt idx = match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT x -> x
  | t -> type_error "text" idx t
let get_int64 stmt idx = match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT x -> x
  | t -> type_error "int" idx t
let get_int64opt stmt idx = match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT x -> Some x
  | Sqlite3.Data.NULL -> None
  | t -> type_error "int or null" idx t
let get_stropt stmt idx = match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT x -> Some x
  | Sqlite3.Data.NULL -> None
  | t -> type_error "text or null" idx t
let get_bool stmt idx = match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT 0L -> false
  | Sqlite3.Data.INT 1L -> true
  | t -> type_error "0 or 1" idx t




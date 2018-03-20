module StringMap = Map.Make(String)
type view = {
  arguments: string StringMap.t;
  mutable session: string option;
  mutable page_status: ViewWeb.page_status option;
  mutable page_body: string option
}

let get_named_argument_opt { arguments } key =
  try Some (StringMap.find key arguments) with Not_found -> None
let enumerate_arguments { arguments } =
  List.map fst (StringMap.bindings arguments)
let get_session_data { session } = session
let set_session_data view session = view.session <- Some session
let reset_session_data view = view.session <- None
let output_page view status body =
  view.page_status <- Some status;
  view.page_body <- Some body
let make ?session arguments =
  { arguments; session; page_status = None; page_body = None }

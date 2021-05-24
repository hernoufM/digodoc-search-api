open Lwt.Infix
open Data_types

let to_api p = Lwt.bind p EzAPIServerUtils.return

let version _params () = to_api (
    Db.get_version () >|= fun v_db_version ->
    Ok { v_db = PConfig.database; v_db_version })

let module_entry ((_params, last_id), pattern) () =  to_api (
    Db.get_modules (Int64.of_int last_id) pattern >|= fun modules ->
    Ok modules)
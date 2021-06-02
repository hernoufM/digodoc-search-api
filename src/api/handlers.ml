open Lwt.Infix
open Data_types

let to_api p = Lwt.bind p EzAPIServerUtils.return

let version _params () = to_api (
    Db.get_version () >|= fun v_db_version ->
    Ok { v_db = PConfig.database; v_db_version })

let package_entries ((_params, last_id), pattern) () =  to_api (
    Db.get_packages (Int64.of_int last_id) pattern >|= fun packages ->
    Ok packages)

let library_entries ((_params, last_id), pattern) () =  to_api (
    Db.get_libraries (Int64.of_int last_id) pattern >|= fun libraries ->
    Ok libraries)

let meta_entries ((_params, last_id), pattern) () =  to_api (
    Db.get_metas (Int64.of_int last_id) pattern >|= fun metas ->
    Ok metas)

let module_entries ((_params, last_id), pattern) () =  to_api (
    Db.get_modules (Int64.of_int last_id) pattern >|= fun modules ->
    Ok modules)

let source_entries ((_params, last_id), pattern) () =  to_api (
    Db.get_sources (Int64.of_int last_id) pattern >|= fun sources ->
    Ok sources)
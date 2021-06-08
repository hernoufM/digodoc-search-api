open Lwt.Infix
open Data_types

let to_api p = Lwt.bind p EzAPIServerUtils.return

let version _params () = to_api (
    Db.get_version () >|= fun v_db_version ->
    Ok { v_db = PConfig.database; v_db_version })

let package_entries (_params, entry_info) () =  to_api (
    Db.get_packages entry_info >|= fun packages ->
    Ok packages)

let library_entries (_params, entry_info) () =  to_api (
    Db.get_libraries entry_info >|= fun libraries ->
    Ok libraries)

let meta_entries (_params, entry_info) () =  to_api (
    Db.get_metas entry_info >|= fun metas ->
    Ok metas)

let module_entries (_params, entry_info) () =  to_api (
    Db.get_modules entry_info >|= fun modules ->
    Ok modules)

let source_entries (_params, entry_info) () =  to_api (
    Db.get_sources entry_info >|= fun sources ->
    Ok sources)

let exec_command ((_params, command), entry_info)  () =  to_api (
    match command with
    | Count entry -> 
        Db.count_entries entry entry_info >|= fun result ->
            Ok {result=string_of_int result})
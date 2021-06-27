open Lwt.Infix
open Data_types
open Utils

let to_api p = Lwt.bind p EzAPIServerUtils.return

let version _params () = to_api (
    Db.get_version () >|= fun v_db_version ->
    Ok { v_db = PConfig.database; v_db_version })

let package_entries (_params, entry_info) () =  to_api (
    Db.get_packages (adjust_entry_info entry_info) >|= fun packages ->
    Ok packages)

let library_entries (_params, entry_info) () =  to_api (
    Db.get_libraries (adjust_entry_info entry_info)  >|= fun libraries ->
    Ok libraries)

let meta_entries (_params, entry_info) () =  to_api (
    Db.get_metas (adjust_entry_info entry_info)  >|= fun metas ->
    Ok metas)

let module_entries (_params, entry_info) () =  to_api (
    Db.get_modules (adjust_entry_info entry_info)  >|= fun modules ->
    Ok modules)

let source_entries (_params, entry_info) () =  to_api (
    Db.get_sources (adjust_entry_info entry_info)  >|= fun sources ->
    Ok sources)

let exec_command ((_params, command), entry_info)  () =  to_api (
    match command with
    | Count entry -> 
        Db.count_entries entry (adjust_entry_info entry_info)  >|= fun result ->
            Ok {result=string_of_int result})

let search  (_params, pattern) () = to_api (
    let%lwt packages = Db.search_packages pattern and
    libraries = Db.search_libraries pattern and
    modules = Db.search_modules pattern 
    in 
        Lwt.return(
            let open List in
            if length packages + length libraries + length modules <= 10 
            then Ok {packages; libraries; modules }
            else Ok {packages=[];libraries=[]; modules=[]}
        )
)
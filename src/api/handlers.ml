open Lwt.Infix
open Data_types
open Utils
open Db

let to_api p = Lwt.bind p EzAPIServerUtils.return

let catch_server_error f = 
    Lwt.catch f
    @@ (fun err -> Printf.eprintf "catch_server_error %s\n" (Printexc.to_string_default err); 
    flush stderr;
    match err with 
    | Search_api_error Invalid_regex -> Printf.eprintf "Invalid_regex\n"; flush stderr; EzAPIServerUtils.Answer.return ~code:500 (Error Invalid_regex)
    | Search_api_error Unknown -> Printf.eprintf "Unknown\n"; flush stderr; EzAPIServerUtils.Answer.return ~code:500 (Error Unknown) 
    | _ -> Printf.eprintf "Another\n"; flush stderr; EzAPIServerUtils.Answer.return ~code:500 (Error Unknown))

let entries (_params, (entry_info)) () = catch_server_error @@ fun () -> to_api (
    Printf.eprintf "Handler entries\n"; flush stderr;
    match entry_info.entry with
    | PACK ->
        Entries.get_packages entry_info >|= fun packages ->
        Ok (Opam packages)
    | LIB ->
        Entries.get_libraries entry_info >|= fun libraries ->
        Ok (Lib libraries)
    | MOD ->
        Entries.get_modules entry_info >|= fun modules ->
        Ok (Mdl modules)
    | META ->
        Entries.get_metas entry_info >|= fun metas ->
        Ok (Meta metas)
    | SRC ->
        Entries.get_sources entry_info >|= fun sources ->
            Printf.eprintf "Ok sources of length %d\n" (List.length sources); flush stderr;
        Ok (Src sources)
)

let elements (_params, element_info) () =  catch_server_error @@ fun () -> to_api (
    Elements.get_conditions_from_rows element_info >>= fun conditions ->
    match element_info.element with
    | VAL -> 
        Elements.get_vals conditions element_info >|= fun vals ->
        Ok (Val vals))

let exec_command ((_params, command), info)  () =  catch_server_error @@ fun () -> to_api (
    Printf.eprintf "Handler commands\n"; flush stderr;
    match command,info with
    | Count,Entry entry_info -> 
        Commands.count_entries entry_info  >|= fun result ->
            Printf.eprintf "Ok exec_command of result %d\n" (result); flush stderr;
            Ok {result=string_of_int result}
    | Count,Element element_info ->
        Elements.get_conditions_from_rows element_info >>= fun conditions ->
        Commands.count_elements conditions element_info  >|= fun result ->
            Ok {result=string_of_int result})

let search  (_params, (pattern:pattern)) () = catch_server_error @@ fun () -> to_api (
    Printf.eprintf "pattern= %s\n" pattern; flush stderr;
    let%lwt packages = Search.search_packages pattern 
    and libraries = Search.search_libraries pattern 
    and modules = Search.search_modules pattern 
    in 
        Lwt.return(
            let open List in
            let l = length in
            if l packages + l libraries + l modules <= 10 
            then Ok {packages; libraries; modules }
            else begin
                let rec take n l = 
                    match n,l with
                    | 0, _ | _, [] -> [] 
                    | n, x::ll -> x::take (n-1) ll 
                in
                    let packages = 
                        if l packages > 3 
                        then take 3 packages
                        else packages
                    and libraries = 
                        if l libraries > 3 
                        then take 3 libraries
                        else libraries
                    and modules =
                        let rest_length = 10 - (l libraries + l packages) in
                        if l modules > rest_length  
                        then take rest_length modules
                        else modules
                    in
                        Ok {packages;libraries; modules}
            end
        )
)
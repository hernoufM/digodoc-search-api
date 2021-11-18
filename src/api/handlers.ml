open Lwt.Infix
open Data_types
open Db

(** Module that defines behaviour for every service from [Services] module. *)

let to_api p = 
    Lwt.catch
        (fun () -> Lwt.bind p EzAPIServerUtils.return)
        (fun err ->
            match err with
            | Search_api_error exn -> EzAPIServerUtils.return ~code:500 (Error exn)
            | _ -> EzAPIServerUtils.return ~code:500 (Error Unknown))
(** [to_api p] executes [p] asynchroniusly and encapsulates promise value to answer [EzAPIServerUtils.Answer.t].
    Catches and encapsulates server errors raised by [p] in order to get error's constructors 
    [Data_types.server_error_type] from client-side. If [p] raises another type of error, than it is 
    converted to [Unknown].*)

let entries (_params, (entry_info)) () = to_api (
    match entry_info.entry with
    | PACK ->
        (* get packages from DB *)
        Entries.get_packages entry_info >|= fun packages ->
        Ok (Opam packages)
    | LIB ->
        (* get libraries from DB *)
        Entries.get_libraries entry_info >|= fun libraries ->
        Ok (Lib libraries)
    | MOD ->
        (* get modules from DB *)
        Entries.get_modules entry_info >|= fun modules ->
        Ok (Mdl modules)
    | META ->
        (* get metas from DB *)
        Entries.get_metas entry_info >|= fun metas ->
        Ok (Meta metas)
    | SRC ->
        (* get sources from DB *)
        Entries.get_sources entry_info >|= fun sources ->
        Ok (Src sources)
)
(** Handler for [Services.entries] service. Looks up entry type from [entry_info] and returns list of corresponding 
    entries respecting [entry_info] constraints. *)

let elements (_params, element_info) () = to_api (
    (* Handling and converting list of condidtions *)
    Elements.get_conditions_from_rows element_info >>= fun conditions ->
    match element_info.element with
    | VAL -> 
        (* get vals from DB *)
        Elements.get_vals conditions element_info >|= fun vals ->
        Ok (Val vals)
)
(** Handler for [Services.elements] service. Looks up element type from [element_info] and returns list of corresponding 
    elements respecting [element_info] constraints. *)

let exec_command ((_params, command), info) () = to_api (
    match command,info with
    (* count entries *)
    | Count,Entry entry_info -> 
        (* executes command *)
        Commands.count_entries entry_info  >|= fun result ->
            Ok {result=string_of_int result}
    (* count elements *)
    | Count,Element element_info ->
        (* Handling and converting list of condidtions *)
        Elements.get_conditions_from_rows element_info >>= fun conditions ->
        (* executes command *)
        Commands.count_elements conditions element_info  >|= fun result ->
            Ok {result=string_of_int result}
)
(** Handler for [Services.commands] service. Looks for filters defined in [info] and executes command. *)

let search  (_params, (pattern:pattern)) () = to_api (
    (* get all entries *)
    let%lwt packages = Search.search_packages pattern 
    and libraries = Search.search_libraries pattern 
    and modules = Search.search_modules pattern 
    in 
        Lwt.return (
            let open List in
            let l = length in
            (* if total number of entries is <= 10 *)
            if l packages + l libraries + l modules <= 10 
            then Ok {packages; libraries; modules }
            else begin
                (* take n elements from list *)
                let rec take n l = 
                    match n,l with
                    | 0, _ | _, [] -> [] 
                    | n, x::ll -> x::take (n-1) ll 
                in
                    let packages = 
                        (* take at most 3 packages *)
                        if l packages > 3
                        then take 3 packages
                        else packages
                    and libraries = 
                        (* take at most 3 libraries *)
                        if l libraries > 3 
                        then take 3 libraries
                        else libraries
                    and modules =
                        (* take the rest for modules *)
                        let rest_length = 10 - (l libraries + l packages) in
                        if l modules > rest_length  
                        then take rest_length modules
                        else modules
                    in
                        Ok {packages;libraries; modules}
            end
        )
)
(** Handler for [Services.commands] service. Looks for specifies pattern and returns at most 10 entries 
    (packages, libraries and modules). *)

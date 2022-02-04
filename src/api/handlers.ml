open Lwt.Infix
open Data_types
open Db
open EzFile.OP


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

let rec take n l =
    match n,l with
    | 0, _ | _, [] -> []
    | n, x::ll -> x::take (n-1) ll
(* take [n] first elements from list *)

let entries (_params, entry_info) () = to_api (
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

let opam_modules ((_params, pattern), pattern_list) () = to_api (
    Entries.get_modules_from_packages pattern pattern_list >|= fun modules ->
        Ok modules
)
(** Handler for [Services.opam_modules] service. Looks for specified pattern and returns list of modules that are atteched
    to one of the opam package defiend in [pattern_list]. *)

let elements (_params, element_info) () = to_api (
    (* Handling and converting list of condidtions *)
    Elements.get_modules_from_conditions element_info >>= fun modules ->
    match element_info.element with
    | VAL ->
        (* get vals from DB *)
        Elements.get_vals modules element_info >|= fun vals ->
            Ok (Val vals)
    | TYPE ->
        (* gets types from DB *)
        Elements.get_types modules element_info >|= fun types ->
            Ok (Type types)
(*
    | CLASS -> Element.get_types modules element_info >|= fun classes ->
            Ok (Class classes)
*)
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
        Elements.get_modules_from_conditions element_info >>= fun modules ->
        (* executes command *)
        Commands.count_elements modules element_info  >|= fun result ->
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
                let np, packages =
                    (* take at most 3 packages *)
                    if l packages > 3
                    then 3, take 3 packages
                    else l packages, packages
                and nl,libraries =
                    (* take at most 3 libraries *)
                    if l libraries > 3
                    then 3, take 3 libraries
                    else l libraries, libraries
                in
                let modules =
                    (* take the rest for modules *)
                    let rest_length = 10 - (np + nl) in
                    if l modules > rest_length
                    then take rest_length modules
                    else modules
                in
                    Ok {packages;libraries; modules}
            end
        )
)
(** Handler for [Services.commands] service. Looks for specified pattern and returns at most 10 entries
    (packages, libraries and modules). *)

open Ez_search.V1.EzSearch

let sources_ocaml_db = ref None
(** Sources DB for ocaml files *)

let sources_dune_db = ref None
(** Sources DB for ocaml files *)

let sources_makefile_db = ref None
(** Sources DB for ocaml files *)

let sources_db_path = PConfig.digodoc_dir // "sources_db"
(** Sources DB path *)

let ocaml_db_name = "ocaml_db"
(** Ocaml files DB name *)

let dune_db_name = "dune_db"
(** Dune files DB name *)

let makefile_db_name = "makefile_db"
(** Makefiles DB name *)

let get_sources_db db : TYPES.db =
  match !db with
  | None -> raise @@ Data_types.search_api_error No_sources_config
  | Some db -> db
(** Returns sources DB. Raises [Search_api_error] if db isn't initialised. *)

let search_sources (_params, sources_search_info) () = to_api (
    let open Ez_search.V1 in
    let rec get_sublist l start n =
        match l,start with
        | [],_ -> []
        | _, 0 -> take n l
        | _::ll, i -> get_sublist ll (i-1) n
    in
    let db =
        match sources_search_info.files with
        | ML -> sources_ocaml_db
        | DUNE -> sources_dune_db
        | MAKEFILE -> sources_makefile_db
    in
    let src_db = get_sources_db db in
    let totaloccs, occurences =
        EzSearch.search_and_count
            ~db:src_db
            ~is_regexp:sources_search_info.is_regex
            ~is_case_sensitive:sources_search_info.is_case_sensitive
            ~ncores:8 (* number of cores of server under domain docs-api.ocaml.pro *)
            sources_search_info.pattern
    in
        (*Printf.eprintf "LINES=%d/%d\n" (List.length occurences) totaloccs; flush stderr;
        let occurences = get_sublist occurences sources_search_info.last_match_id 20 in
        List.iter (fun occ ->
            let occ_file = EzSearch.occurrence_file ~db:src_db occ in
            let occ_line_num = EzSearch.occurrence_line ~db:src_db occ_file in
            let occ_context = EzSearch.occurrence_context ~db:src_db ~line:occ_line_num ~max:0 occ_file in

            Printf.eprintf "pos=%d line=%s\n" occ_line_num occ_context.curr_line; flush stderr)
            occurences;
        Lwt.return (Ok {totaloccs=0; occs=[{opamname=""; srcpath=""; filename=""; occpos=0; occline=""; occpath=""}]})*)
        let occurences = get_sublist occurences sources_search_info.last_match_id 20 in
        let%lwt occurences_info =
            Lwt_list.map_s (fun occ ->
                    let occ_file = EzSearch.occurrence_file ~db:src_db occ in
                    let occ_line_num = EzSearch.occurrence_line ~db:src_db occ_file in
                    let occ_context = EzSearch.occurrence_context ~db:src_db ~line:occ_line_num ~max:0 occ_file in
                    let opam_name,_ = EzString.cut_at occ_file.TYPES.occ_file.file_entry '.' in
                    let%lwt src_info = Db.Sources_search.get_src_info opam_name in
                    Lwt.return (occ_file,occ_line_num,occ_context,src_info)
                )
                occurences in
        let occs =
            List.map (fun (occ_file, occ_line_num, occ_context,(opamname,srcpath)) ->
                    let open TYPES in
                    let filename = occ_file.occ_file.file_name
                    and occpos = occ_line_num
                    and occline = occ_context.curr_line in
                    let occpath = srcpath // filename // "index.html#L" ^ string_of_int occpos in
                    {opamname; srcpath; filename; occpos; occline; occpath}
                )
                occurences_info in
        Lwt.return (Ok {totaloccs; occs})
)
(** Handler for [Services.search_sources] service. Makes a search with [ez_search] in specific DB by applying filters from [sources_search_info]. *)

open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types

(** Module [Db] regroups all the DB requests divided into module for every handler.
    Request are sended to PostgreSQL database. Every function that is supposed to be an handler's entry point
    catches PostgreSQL errors with [Misc_db.catch_db_error]. Functions use ```pgocaml_ppx``` that defines special syntax
    for SQL requests. *)

let get_opam_by_name dbh opam_name =
  [%pgsql.object dbh "select opam_name,opam_version 
                      from opam_index 
                      where opam_name = $opam_name"]
(** Get package row from DB that has specified name. *)
 
let get_mdl_by_id dbh mdl_id =
  [%pgsql.object dbh "select * 
                      from module_index 
                      where mdl_id=$mdl_id"]
(** Get module row from DB that has specified id. *)

let get_libs_by_mdl_id dbh mdl_id =
  [%pgsql.object dbh "select mdl_lib 
                      from module_libraries 
                      where mdl_id=$mdl_id"]
(* Get name of libraries to which module with specified id is included. *)

let get_mdls_by_name dbh mdl =
  [%pgsql.object dbh "select mdl_name,mdl_opam
                      from module_index 
                      where mdl_name=$mdl"]
(** Get module rows by module name *)

module Entries = struct

  let get_packages {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () -> 
        [%pgsql.object dbh "select * 
                        from 
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id 
                          from opam_index 
                          where opam_name ~* $pattern and opam_name ~* $starts_with) result 
                        where result.row_id>${Int64.of_int last_id} 
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        >|= packages_of_rows
  (** Get 50 first packages starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Package should match [starts_with] regex expression, that indicates the first letter of the name.
      - Package should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_libraries {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh ->  catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select * 
                        from 
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(lib_name)) as row_id 
                          from library_index 
                          where lib_name ~* $pattern and lib_name ~* $starts_with) result 
                        where result.row_id>${Int64.of_int last_id} 
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        in 
          Lwt_list.map_s 
            (fun row ->
              let opam_name = row#lib_opam in 
              let%lwt opam_row = get_opam_by_name dbh opam_name in
                Lwt.return (library_of_row row opam_row))
            rows
  (** Get 50 first libraries starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Library should match [starts_with] regex expression, that indicates the first letter of the name.
      - Library should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_metas {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select * 
                        from 
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(meta_name)) as row_id 
                          from meta_index 
                          where meta_name ~* $pattern and meta_name ~* $starts_with) result 
                        where result.row_id>${Int64.of_int last_id} 
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        in 
          Lwt_list.map_s 
            (fun row ->
              let opam_name = row#meta_opam in 
              let%lwt opam_row = get_opam_by_name dbh opam_name in
                Lwt.return (meta_of_row row opam_row))
            rows
  (** Get 50 first metas starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Meta should match [starts_with] regex expression, that indicates the first letter of the name.
      - Meta should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_modules {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select * 
                              from 
                                (select *, ROW_NUMBER () OVER (ORDER BY UPPER(mdl_name)) as row_id 
                                 from module_index 
                                 where mdl_name ~* $pattern and mdl_name ~* $starts_with) result 
                              where result.row_id>${Int64.of_int last_id} 
                              and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        in
          Lwt_list.map_s 
            (fun row ->
              let mdl_id = row#mdl_id in
              let opam_name = row#mdl_opam in 
              let%lwt opam_row = get_opam_by_name dbh opam_name in
              let%lwt lib_rows = get_libs_by_mdl_id dbh mdl_id in
              Lwt.return (module_of_row row opam_row lib_rows))
            rows
  (** Get 50 first modules starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Module should match [starts_with] regex expression, that indicates the first letter of the name.
      - Module should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_sources {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        [%pgsql.object dbh "select * 
                        from 
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id 
                          from opam_index 
                          where opam_name ~* $pattern and opam_name ~* $starts_with) result 
                        where result.row_id>${Int64.of_int last_id} 
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        >|= sources_of_rows
  (** Get 50 first sources starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Source should match [starts_with] regex expression, that indicates the first letter of the name.
      - Source should match [pattern] regex expression, that describes case insensetive substring of the name. *)
end
(** Module that regroups all DB requests for [Handlers.entries] handler. *)

module Elements = struct 
  (* TODO: rewrite and test *)
  let get_conditions_from_rows {conditions; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () -> 
        let mdls = ref []
        and packs = ref [] in
        let%lwt () = 
          Lwt_list.iter_s 
            (fun cond ->
              match cond with 
              | In_opam opam -> 
                let%lwt pkgs = get_opam_by_name dbh opam in 
                let pkgs = 
                  List.map 
                    (fun opam-> 
                        Cond.make_opam_cond 
                        (name_of_opam opam#opam_name opam#opam_version)
                    ) 
                    pkgs 
                in
                  packs:= Cond.opam_union !packs pkgs ;
                  Lwt.return_unit
              | In_mdl mdl -> 
                let%lwt mods = get_mdls_by_name dbh mdl in 
                let%lwt mods = 
                  Lwt_list.map_s 
                    (fun mdl-> 
                      let%lwt opam = get_opam_by_name dbh mdl#mdl_opam in
                      let opam = List.hd opam in
                      Cond.make_mdl_cond 
                        mdl#mdl_name
                        (name_of_opam opam#opam_name opam#opam_version)
                      |> Lwt.return
                    ) 
                    mods 
                in
                  mdls:= Cond.mdl_union !mdls mods;
                  Lwt.return_unit
            )
            conditions
        in Lwt.return (!packs, !mdls)
    

  let get_vals conditions {last_id; pattern; _} =
    with_dbh >>> fun dbh -> catch_db_error @@
    fun () -> 
      let%lwt rows = [%pgsql.object dbh "select * 
                        from 
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(mdl_ident)) as row_id 
                          from module_vals 
                          where mdl_ident ~* $pattern) result 
                        where result.row_id>${Int64.of_int last_id} 
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
      in
        Lwt_list.filter_map_s 
          (fun row ->
            let opam_name = row#mdl_opam_name in 
            let%lwt opam_row = get_opam_by_name dbh opam_name in
            let%lwt mdl_row = get_mdl_by_id dbh row#mdl_id in
            Lwt.return (val_of_row_opt conditions row opam_row mdl_row)
          )
          rows
  (** Get 50 first ocaml values starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Value should respect at least 1 package constraint and at least 1 module constaint if they are presented. Constraint
      describes to which entry this ocaml value is included.
      - Value should match [pattern] regex expression, that describes case insensetive substring of the name. *)
end
(** Module that regroups all DB requests for [Handlers.elements] handler. *)

module Commands = struct

  let count_entries {entry; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        begin 
          match entry with
          (* count packages *)
          | PACK -> [%pgsql dbh "select count(*) as n
                                      from opam_index 
                                      where opam_name ~* $pattern and opam_name ~* $starts_with"]
          (* count libraries *)
          | LIB ->  [%pgsql dbh "select count(*) as n 
                                      from library_index 
                                      where lib_name ~* $pattern and lib_name ~* $starts_with"]
          (* count metas *)
          | META -> [%pgsql dbh "select count(*) as n
                                      from meta_index 
                                      where meta_name ~* $pattern and meta_name ~* $starts_with"]
          (* count modules *)
          | MOD ->  [%pgsql dbh "select count(*) as n
                                      from module_index 
                                      where mdl_name ~* $pattern and mdl_name ~* $starts_with"]
          (* count sources *)
          | SRC ->  [%pgsql dbh "select count(*) as n
                                      from opam_index 
                                      where opam_name ~* $pattern and opam_name ~* $starts_with"]
        end
        >|= count_from_row
  (** Count number of entries in DB for specific entry type mentionned in [entry] field. It should follow those conditions :
      - Entry name should match [starts_with] regex expression, that indicates the first letter of the name.
      - Entry name should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let count_elements conditions {element; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      let%lwt elements = 
        match element with
        | VAL -> [%pgsql.object dbh "select * 
                            from module_vals 
                            where mdl_ident ~* $pattern "]
      in 
        Lwt_list.fold_right_s 
          (fun element cpt ->
            let opam_name = element#mdl_opam_name in 
            let%lwt opam_row = get_opam_by_name dbh opam_name in
            let%lwt mdl_row = get_mdl_by_id dbh element#mdl_id in
            Lwt.return (count_elements_in_rows conditions opam_row mdl_row element cpt)
          )
          elements
          0
  (** Count number of elements in DB for specific element type mentionned in [element] field. It should follow those conditions :
      - Element name should respect at least 1 package constraint and at least 1 module constaint if they are presented. Constraint
      describes to which entry this element is included.
      - Element name should match [pattern] regex expression, that describes case insensetive substring of the name. *)
end
(** Module that regroups all DB requests for [Handlers.exec_command] handler. *)

module Search = struct 

  let search_packages pattern =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        [%pgsql.object dbh "select *
                          from opam_index 
                          where opam_name ~* $pattern"]
        >|= packages_of_rows 
  (** Get all the packages that match specified regex pattern. *)

  let search_libraries pattern =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select *
                                from library_index 
                                where lib_name ~* $pattern"]
        in 
          Lwt_list.map_s 
            (fun row ->
              let opam_name = row#lib_opam in 
              let%lwt opam_row = get_opam_by_name dbh opam_name in
              Lwt.return (library_of_row row opam_row))
            rows
  (** Get all the libraries that match specified regex pattern. *)

  let search_modules pattern =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select *
                                from module_index 
                                where mdl_name ~* $pattern"] 
        in
          Lwt_list.map_s 
            (fun row ->
              let mdl_id = row#mdl_id in
              let opam_name = row#mdl_opam in 
              let%lwt opam_row = get_opam_by_name dbh opam_name in
              let%lwt lib_rows = get_libs_by_mdl_id dbh mdl_id in
              Lwt.return (module_of_row row opam_row lib_rows))
            rows
  (** Get all the modules that match specified regex pattern. *)
end
(** Module that regroups all DB requests for [Handlers.search] handler. *)
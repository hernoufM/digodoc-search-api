open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types


let get_opam_info dbh opam_name =
  [%pgsql.object dbh "select opam_name,opam_version 
                      from opam_index 
                      where opam_name = $opam_name"]

let get_mdl_info_id dbh mdl_id =
  [%pgsql.object dbh "select * 
                      from module_index 
                      where mdl_id=$mdl_id"]

let get_libs_mdl dbh mdl_id =
  [%pgsql.object dbh "select mdl_lib 
                      from module_libraries 
                      where mdl_id=$mdl_id"]

module Entries = struct

  let get_packages {last_id; starts_with; pattern} =
    with_dbh >>> fun dbh ->
    [%pgsql.object dbh "select * 
                        from 
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id 
                          from opam_index 
                          where opam_name ~* $pattern and opam_name ~* $starts_with) result 
                        where result.row_id>${Int64.of_int last_id} 
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
    >|= packages_of_rows

  let get_libraries {last_id; starts_with; pattern} =
    with_dbh >>> fun dbh ->
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
          let%lwt opam_row = get_opam_info dbh opam_name in
            Lwt.return (library_of_row row opam_row))
        rows

  let get_metas {last_id; starts_with; pattern} =
    with_dbh >>> fun dbh ->
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
          let%lwt opam_row = get_opam_info dbh opam_name in
            Lwt.return (meta_of_row row opam_row))
        rows



  let get_modules {last_id; starts_with; pattern} =
    with_dbh >>> fun dbh ->
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
          let%lwt opam_row = get_opam_info dbh opam_name in
          let%lwt lib_rows = get_libs_mdl dbh mdl_id in
          Lwt.return (module_of_row row opam_row lib_rows))
        rows

  let get_sources {last_id; starts_with; pattern} =
    with_dbh >>> fun dbh ->
    [%pgsql.object dbh "select * 
                        from 
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id 
                          from opam_index 
                          where opam_name ~* $pattern and opam_name ~* $starts_with) result 
                        where result.row_id>${Int64.of_int last_id} 
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
      >|= sources_of_rows

end

module Elements = struct 
  
  let get_opam_name dbh opam = 
    [%pgsql.object dbh "select opam_name,opam_version
                      from opam_index 
                      where opam_name=$opam"]

  let get_mdl_name dbh mdl =
          [%pgsql.object dbh "select mdl_name,mdl_opam
                      from module_index 
                      where mdl_name=$mdl"]


  let get_conditions_from_rows {conditions} =
    with_dbh >>> fun dbh ->
    let mdls = ref []
    and packs = ref [] in
    let%lwt () = 
      Lwt_list.iter_s (fun cond ->
                match cond with 
                | In_opam opam -> 
                  let%lwt pkgs = get_opam_name dbh opam in 
                  let pkgs = List.map (fun opam-> 
                        Cond.make_opam_cond 
                          (name_of_opam opam#opam_name opam#opam_version)
                      ) 
                      pkgs in
                  packs:= Cond.opam_union !packs pkgs ;
                  Lwt.return_unit
                | In_mdl mdl -> 
                  let%lwt mods = get_mdl_name dbh mdl in 
                  let%lwt mods = 
                    Lwt_list.map_s (fun mdl-> 
                        let%lwt opam = get_opam_info dbh mdl#mdl_opam in
                        let opam = List.hd opam in
                        Cond.make_mdl_cond 
                          mdl#mdl_name
                          (name_of_opam opam#opam_name opam#opam_version)
                        |> Lwt.return
                      ) 
                    mods in
                  mdls:= Cond.mdl_union !mdls mods;
                  Lwt.return_unit)
        conditions
    in Lwt.return (!packs, !mdls)
    

  let get_vals conditions {last_id; pattern; mode} =
    with_dbh >>> fun dbh ->
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
          let%lwt opam_row = get_opam_info dbh opam_name in
          let%lwt mdl_row = get_mdl_info_id dbh row#mdl_id in
          Lwt.return (val_of_row_opt conditions row opam_row mdl_row))
        rows
end

module Commands = struct

  let count_entries {entry; starts_with; pattern} =
    with_dbh >>> fun dbh ->
      begin 
        match entry with
        | PACK -> [%pgsql dbh "select count(*) as n
                                      from opam_index 
                                      where opam_name ~* $pattern and opam_name ~* $starts_with"]
        | LIB ->  [%pgsql dbh "select count(*) as n 
                                      from library_index 
                                      where lib_name ~* $pattern and lib_name ~* $starts_with"]
        | META -> [%pgsql dbh "select count(*) as n
                                      from meta_index 
                                      where meta_name ~* $pattern and meta_name ~* $starts_with"]
        | MOD ->  [%pgsql dbh "select count(*) as n
                                      from module_index 
                                      where mdl_name ~* $pattern and mdl_name ~* $starts_with"]
        | SRC ->  [%pgsql dbh "select count(*) as n
                                      from opam_index 
                                      where opam_name ~* $pattern and opam_name ~* $starts_with"]
      end;
    >|= count_from_row


  let count_elements conditions {element; mode; pattern} =
    with_dbh >>> fun dbh ->
    let%lwt elements = 
      match element with
      | VAL -> [%pgsql.object dbh "select * 
                            from module_vals 
                            where mdl_ident ~* $pattern "]
    in 
        Lwt_list.fold_right_s (fun element cpt ->
            let opam_name = element#mdl_opam_name in 
            let%lwt opam_row = get_opam_info dbh opam_name in
            let%lwt mdl_row = get_mdl_info_id dbh element#mdl_id in
            Lwt.return (count_elements_in_rows conditions opam_row mdl_row element cpt)
          )
          elements
          0
 
end
module Search = struct 

  let search_packages pattern =
    with_dbh >>> fun dbh ->
      [%pgsql.object dbh "select *
                          from opam_index 
                          where opam_name ~* $pattern"]
      >|= packages_of_rows 

  let search_libraries pattern =
    with_dbh >>> fun dbh ->
    let%lwt rows = [%pgsql.object dbh "select *
                                from library_index 
                                where lib_name ~* $pattern"]
    in 
      Lwt_list.map_s 
        (fun row ->
          let opam_name = row#lib_opam in 
          let%lwt opam_row = get_opam_info dbh opam_name in
            Lwt.return (library_of_row row opam_row))
        rows

  let search_modules pattern =
    with_dbh >>> fun dbh ->
    let%lwt rows = [%pgsql.object dbh "select *
                                from module_index 
                                where mdl_name ~* $pattern"] in
    Lwt_list.map_s 
      (fun row ->
        let mdl_id = row#mdl_id in
        let opam_name = row#mdl_opam in 
        let%lwt opam_row = get_opam_info dbh opam_name in
        let%lwt lib_rows = get_libs_mdl dbh mdl_id in
          Lwt.return (module_of_row row opam_row lib_rows))
      rows
end
open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types

let get_version () =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "select value from ezpg_info where name = 'version'"]
  >|= version_of_rows

let get_opam_info dbh opam_name =
  [%pgsql.object dbh "select opam_name,opam_version 
                      from opam_index 
                      where opam_name = $opam_name"]

let get_lib_mdl dbh mdl_id =
  [%pgsql.object dbh "select mdl_lib 
                      from module_libraries 
                      where mdl_id=$mdl_id"]

let get_packages {last_id; pattern} =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  [%pgsql.object dbh "select * 
                      from 
                        (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id 
                        from opam_index 
                        where opam_name ilike ${\"%\" ^ pattern ^ \"%\"}) result 
                      where result.row_id>$last_id 
                      and result.row_id < ${Int64.add last_id (Int64.of_int 50)}"]
  >|= packages_of_rows

let get_libraries {last_id; pattern} =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  let%lwt rows = [%pgsql.object dbh "select * 
                      from 
                        (select *, ROW_NUMBER () OVER (ORDER BY UPPER(lib_name)) as row_id 
                        from library_index 
                        where lib_name ilike ${\"%\" ^ pattern ^ \"%\"}) result 
                      where result.row_id>$last_id 
                      and result.row_id < ${Int64.add last_id (Int64.of_int 50)}"]
  in 
    Lwt_list.map_s 
      (fun row ->
        let opam_name = row#lib_opam in 
        let%lwt opam_row = get_opam_info dbh opam_name in
          Lwt.return (library_of_row row opam_row))
      rows

let get_metas {last_id; pattern} =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  let%lwt rows = [%pgsql.object dbh "select * 
                      from 
                        (select *, ROW_NUMBER () OVER (ORDER BY UPPER(meta_name)) as row_id 
                        from meta_index 
                        where meta_name ilike ${\"%\" ^ pattern ^ \"%\"}) result 
                      where result.row_id>$last_id 
                      and result.row_id < ${Int64.add last_id (Int64.of_int 50)}"]
  in 
    Lwt_list.map_s 
      (fun row ->
        let opam_name = row#meta_opam in 
        let%lwt opam_row = get_opam_info dbh opam_name in
          Lwt.return (meta_of_row row opam_row))
      rows



let get_modules {last_id; pattern} =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  let%lwt rows = [%pgsql.object dbh "select * 
                          from 
                            (select *, ROW_NUMBER () OVER (ORDER BY UPPER(mdl_name)) as row_id 
                             from module_index 
                             where mdl_name ilike ${\"%\" ^ pattern ^ \"%\"}) result 
                          where result.row_id>$last_id 
                            and result.row_id < ${Int64.add last_id (Int64.of_int 50)}"] in
  Lwt_list.map_s 
    (fun row ->
      let mdl_id = row#mdl_id in
      let opam_name = row#mdl_opam in 
      let%lwt opam_row = get_opam_info dbh opam_name in
      let%lwt lib_rows = get_lib_mdl dbh mdl_id in
        Lwt.return (module_of_row row opam_row lib_rows))
    rows

let get_sources {last_id; pattern} =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  [%pgsql.object dbh "select * 
                      from 
                        (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id 
                        from opam_index 
                        where opam_name ilike ${\"%\" ^ pattern ^ \"%\"}) result 
                      where result.row_id>$last_id 
                      and result.row_id < ${Int64.add last_id (Int64.of_int 50)}"] 
    >|= sources_of_rows


let count_entries entry {last_id; pattern} =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
    begin 
      match entry with
      | PACK -> [%pgsql dbh "select count(*) as n
                                    from opam_index 
                                    where opam_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
      | LIB ->  [%pgsql dbh "select count(*) as n 
                                    from library_index 
                                    where lib_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
      | META -> [%pgsql dbh "select count(*) as n
                                    from meta_index 
                                    where meta_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
      | MOD ->  [%pgsql dbh "select count(*) as n
                                    from module_index 
                                    where mdl_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
      | SRC ->  [%pgsql dbh "select count(*) as n
                                    from opam_index 
                                    where opam_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
    end;
    >|= count_from_row

let search_packages pattern =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
    [%pgsql.object dbh "select *
                        from opam_index 
                        where opam_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
    >|= packages_of_rows

let search_libraries pattern =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  let%lwt rows = [%pgsql.object dbh "select *
                              from library_index 
                              where lib_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
  in 
    Lwt_list.map_s 
      (fun row ->
        let opam_name = row#lib_opam in 
        let%lwt opam_row = get_opam_info dbh opam_name in
          Lwt.return (library_of_row row opam_row))
      rows

let search_modules pattern =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  let%lwt rows = [%pgsql.object dbh "select *
                              from module_index 
                              where mdl_name ilike ${\"%\" ^ pattern ^ \"%\"}"] in
  Lwt_list.map_s 
    (fun row ->
      let mdl_id = row#mdl_id in
      let opam_name = row#mdl_opam in 
      let%lwt opam_row = get_opam_info dbh opam_name in
      let%lwt lib_rows = get_lib_mdl dbh mdl_id in
        Lwt.return (module_of_row row opam_row lib_rows))
    rows

(*let search_entries pattern =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
    let pack_rows = [%pgsql dbh "select *
                                    from opam_index 
                                    where opam_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
    and lib_rows = [%pgsql dbh "select *
                                    from library_index 
                                    where lib_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
    and mdl_rows =  [%pgsql dbh "select *
                                    from module_index 
                                    where mdl_name ilike ${\"%\" ^ pattern ^ \"%\"}"]
    in
      *)
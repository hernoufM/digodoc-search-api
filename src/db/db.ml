open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types

let get_version () =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "select value from ezpg_info where name = 'version'"]
  >|= version_of_rows

let get_modules last_id pattern =
  let pattern = if pattern = "~" then "" else pattern in
  with_dbh >>> fun dbh ->
  let%lwt rows = [%pgsql.object dbh "select * 
                          from 
                            (select *, ROW_NUMBER () OVER (ORDER BY mdl_name) as row_id 
                             from module_index 
                             where mdl_name like ${\"%\" ^ pattern ^ \"%\"}) result 
                          where result.row_id>$last_id 
                            and result.row_id < ${Int64.add last_id (Int64.of_int 50)}"] in
  Lwt_list.map_s 
    (fun row ->
      let id = row#mdl_id in 
      let%lwt rows_lib = [%pgsql.object dbh "select * 
                                              from module_libraries 
                                              where mdl_id=$id"] in
        Lwt.return (entries_of_rows row rows_lib))
    rows

(*let get_modules last_id pattern =
  with_dbh >>> fun dbh ->
  let lib_row = object method mdl_id = 20 method lib_name = "toto" method lib_path = pattern end in 
  let%lwt row = 
  [%pgsql.object dbh "select * 
                          from 
                            (select *, ROW_NUMBER () OVER (ORDER BY mdl_name) as row_id 
                             from module_index ) result 
                          where result.row_id=5 "] in
  
  Lwt.return (entries_of_rows (List.hd row) [lib_row])*)

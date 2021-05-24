open Data_types

let version_of_rows = function [ Some v ] -> Int32.to_int v | _ -> 0

let module_libs_of_row rows =
    List.map
        (fun row -> (row#lib_path,row#lib_name))
        rows

let entries_of_rows row libs_row =
    let libs = module_libs_of_row libs_row in
    {mdl_id=row#mdl_id;
    search_id=row#search_id; 
    html_path=row#html_path;
    mdl_name=row#mdl_name; 
    opam_package=row#opam_package; 
    mdl_opam_name=row#mdl_opam_name; 
    mdl_opam_version=row#mdl_opam_version;
    libs=libs}
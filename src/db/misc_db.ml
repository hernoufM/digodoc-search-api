open Data_types

let version_of_rows = function [ Some v ] -> Int32.to_int v | _ -> 0

let path_of_opam opam_name opam_version =
    Printf.sprintf "OPAM.%s.%s/index.html" opam_name opam_version

let name_of_opam opam_name opam_version = 
    Printf.sprintf "%s.%s" opam_name opam_version

let path_of_lib lib_name opam_name opam_version =
  Printf.sprintf "LIBRARY.%s@%s.%s/index.html"
    lib_name opam_name opam_version

let path_of_meta meta_name opam_name opam_version =
  Printf.sprintf "META.%s@%s.%s/index.html"
    meta_name opam_name opam_version

let path_of_src opam_name opam_version = 
    Printf.sprintf "../sources/%s.%s/index.html"
        opam_name opam_version

let packages_of_rows rows = 
    List.map (function row ->
        let opam_path = path_of_opam row#opam_name row#opam_version in
        {opam_name = row#opam_name;
         opam_path;
         opam_version = row#opam_version;
         opam_synopsis = row#opam_synopsis}
    ) rows

let library_of_row row opam_row =
    let opam_row = List.hd opam_row in 
    let lib_path = path_of_lib row#lib_name opam_row#opam_name opam_row#opam_version 
    and lib_opam_path = path_of_opam opam_row#opam_name opam_row#opam_version 
    and lib_opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    in
        {lib_name = row#lib_name;
        lib_path;
        lib_opam;
        lib_opam_path}

let meta_of_row row opam_row =
    let opam_row = List.hd opam_row in 
    let meta_path = path_of_meta row#meta_name opam_row#opam_name opam_row#opam_version 
    and meta_opam_path = path_of_opam opam_row#opam_name opam_row#opam_version 
    and meta_opam = name_of_opam opam_row#opam_name opam_row#opam_version
    in
        {meta_name = row#meta_name;
        meta_path;
        meta_opam;
        meta_opam_path} 

let module_of_row row opam_row libs_row =
    let opam_row = List.hd opam_row in 
    let mdl_opam_path = path_of_opam opam_row#opam_name opam_row#opam_version 
    and mdl_opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    and mdl_libs = 
        List.map 
            (fun row -> 
                let lib_path = path_of_lib row#mdl_lib opam_row#opam_name opam_row#opam_version in
                (row#mdl_lib, lib_path))
            libs_row
    in
        {mdl_path=row#mdl_path;
        mdl_name=row#mdl_name; 
        mdl_opam_path; 
        mdl_opam;
        mdl_libs}

let sources_of_rows rows =
    List.map (function row -> 
        let src_name = row#opam_name
        and src_path = path_of_src row#opam_name row#opam_version
        and src_opam = name_of_opam row#opam_name row#opam_version
        and src_opam_path = path_of_opam row#opam_name row#opam_version in
        {
            src_name;
            src_path;
            src_opam;
            src_opam_path
        }
    ) rows
    
let count_from_row = function [ Some v ] -> Int64.to_int v | _ -> 0
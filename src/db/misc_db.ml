open Data_types

let version_of_rows = function [ Some v ] -> Int32.to_int v | _ -> 0

let path_of_opam opam_name opam_version =
    Printf.sprintf "docs/OPAM.%s.%s/index.html" opam_name opam_version

let name_of_opam opam_name opam_version = 
    Printf.sprintf "%s.%s" opam_name opam_version

let path_of_lib lib_name opam_name opam_version =
  Printf.sprintf "docs/LIBRARY.%s@%s.%s/index.html"
    lib_name opam_name opam_version

let path_of_meta meta_name opam_name opam_version =
  Printf.sprintf "docs/META.%s@%s.%s/index.html"
    meta_name opam_name opam_version

let path_of_src opam_name opam_version = 
    Printf.sprintf "sources/%s.%s/index.html"
        opam_name opam_version

let packages_of_rows rows = 
    List.map (function row ->
        let path = path_of_opam row#opam_name row#opam_version in
        {name = row#opam_name;
         path;
         version = row#opam_version;
         synopsis = row#opam_synopsis}
    ) rows

let library_of_row row opam_row : lib_entry =
    let opam_row = List.hd opam_row in 
    let path = path_of_lib row#lib_name opam_row#opam_name opam_row#opam_version 
    and opampath = path_of_opam opam_row#opam_name opam_row#opam_version 
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    in
        {name = row#lib_name;
        path;
        opam;
        opampath}

let meta_of_row row opam_row : meta_entry =
    let opam_row = List.hd opam_row in 
    let path = path_of_meta row#meta_name opam_row#opam_name opam_row#opam_version 
    and opampath = path_of_opam opam_row#opam_name opam_row#opam_version 
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version
    in
        {name = row#meta_name;
        path;
        opam;
        opampath} 

let module_of_row row opam_row libs_row =
    let opam_row = List.hd opam_row in 
    let opampath = path_of_opam opam_row#opam_name opam_row#opam_version 
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    and libs = 
        List.map 
            (fun row -> 
                let lib_path = path_of_lib row#mdl_lib opam_row#opam_name opam_row#opam_version in
                (row#mdl_lib, lib_path))
            libs_row
    in
        {path=row#mdl_path;
        name=row#mdl_name; 
        opampath; 
        opam;
        libs}

let sources_of_rows rows : sources =
    List.map (function row -> 
        let name = row#opam_name
        and path = path_of_src row#opam_name row#opam_version
        and opam = name_of_opam row#opam_name row#opam_version
        and opampath = path_of_opam row#opam_name row#opam_version in
        {
            name;
            path;
            opam;
            opampath
        }
    ) rows
    
let count_from_row = function [ Some v ] -> Int64.to_int v | _ -> 0

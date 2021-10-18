open Data_types

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

module Cond : sig 
    type opam_cond
    type mdl_cond
    val make_opam_cond : string -> opam_cond
    val make_mdl_cond : string -> string -> mdl_cond   
    val opam_union : opam_cond list -> opam_cond list -> opam_cond list
    val mdl_union : mdl_cond list -> mdl_cond list -> mdl_cond list
    val check_validity : opam_cond list -> mdl_cond list -> bool
    val respect_opam_conditions : opam_cond -> opam_cond list -> bool
    val respect_mdl_conditions : mdl_cond -> mdl_cond list -> bool

    end  = struct 

    type opam_cond = string
    type mdl_cond = string * string

    let make_opam_cond opam = opam

    let make_mdl_cond mdl opam = (mdl,opam)

    let eq_pack p1 p2 = String.equal p1 p2

    let eq_mdl (mdl1,opam1) (mdl2,opam2) =
        String.equal mdl1 mdl2
        && String.equal opam1 opam2

    let mem_pack p = List.exists (eq_pack p)

    let mem_mdl m = List.exists (eq_mdl m)

    let opam_union packs1 packs2 =
        let union = ref packs1 in 
        List.iter (fun pack2 ->
            if not @@ mem_pack pack2 packs1
            then union := pack2::!union
            )
            packs2;
        !union

    let mdl_union mdls1 mdls2 =
        let union = ref mdls1 in 
        List.iter (fun mdl2 ->
            if not @@ mem_mdl mdl2 mdls1
            then union := mdl2::!union
            )
            mdls2;
        !union

    let check_validity opam_conds mdl_conds =
        List.for_all (fun (mdl,opam) ->
                if opam_conds <> []
                then mem_pack (make_opam_cond opam) opam_conds
                else true
            )
            mdl_conds

    let respect_opam_conditions opam opam_conds =
        match opam_conds with
        | [] -> true
        | _ -> mem_pack opam opam_conds

    let respect_mdl_conditions mdl mdl_conds =
        match mdl_conds with
        | [] -> true
        | _ -> mem_mdl mdl mdl_conds

end

let val_of_row (in_packs, in_mdls) row opam_row mdl_row = 
    let open Cond in
    let opam_row = List.hd opam_row 
    and mdl_row = List.hd mdl_row in 
    let opampath = path_of_opam opam_row#opam_name opam_row#opam_version
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    and mdlpath = mdl_row#mdl_path
    and mdl = mdl_row#mdl_name in
    if respect_opam_conditions (make_opam_cond opam) in_packs 
       && respect_mdl_conditions (make_mdl_cond mdl opam) in_mdls 
    then Some {
                ident = row#mdl_ident;
                value = row#mdl_val;
                mdl;
                mdlpath;
                opam;
                opampath
            }
    else None

let count_from_row = function [ Some v ] -> Int64.to_int v | _ -> 0

let count_elements_in_rows (in_packs, in_mdls) opam_row mdl_row row cpt =
    let open Cond in
    let opam_row = List.hd opam_row 
    and mdl_row = List.hd mdl_row in
    let opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    and mdl = mdl_row#mdl_name in
    if respect_opam_conditions (make_opam_cond opam) in_packs 
       && respect_mdl_conditions (make_mdl_cond mdl opam) in_mdls 
    then cpt + 1
    else cpt
 
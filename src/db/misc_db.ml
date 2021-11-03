open Data_types

let catch_db_error f =
    Lwt.catch f
    @@ (fun err ->
        (* TODO: Catch correctly PGOCaml.PostgreSQL_Error *)
        Printf.eprintf "catch_db_error\n"; flush stderr;
        match err with
        | Db_lwt.PGOCaml.PostgreSQL_Error _ -> (Printf.eprintf "PostgreSQL_Error\n"; flush stderr;
        Lwt.fail @@ search_api_error Invalid_regex)
        | exn -> Printf.eprintf "Another error : %s\n" (Printexc.to_string exn); 
            flush stderr;Lwt.fail @@ search_api_error Unknown)
(** Catches DB excetpions and raises [Search_api_error] coresponding to DB error *)

let path_of_opam opam_name opam_version =
    Printf.sprintf "docs/OPAM.%s.%s/index.html" opam_name opam_version
(** Computes path to package documentation *)

let name_of_opam opam_name opam_version = 
    Printf.sprintf "%s.%s" opam_name opam_version
(** Computes name of package *)

let path_of_lib lib_name opam_name opam_version =
  Printf.sprintf "docs/LIBRARY.%s@%s.%s/index.html"
    lib_name opam_name opam_version
(** Computes path to library documentation *)

let path_of_meta meta_name opam_name opam_version =
  Printf.sprintf "docs/META.%s@%s.%s/index.html"
    meta_name opam_name opam_version
(** Computes path to meta documentation *)

let path_of_src opam_name opam_version = 
    Printf.sprintf "sources/%s.%s/index.html"
        opam_name opam_version
(** Computes path to source explorer for a package *)

let packages_of_rows rows = 
    List.map (function row ->
        let path = path_of_opam row#opam_name row#opam_version in
        {name = row#opam_name;
         path;
         version = row#opam_version;
         synopsis = row#opam_synopsis}
    ) rows
(** Creates [Data_types.packages] from DB rows *)

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
(** Creates [Data_types.lib_entry] from library and its package DB rows *)

let meta_of_row row opam_row : meta_entry =
    let opam_row = List.hd opam_row in 
    let path = path_of_meta row#meta_name opam_row#opam_name opam_row#opam_version 
    and opampath = path_of_opam opam_row#opam_name opam_row#opam_version 
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version
    in
        {namemeta = row#meta_name;
        path;
        opam;
        opampath} 
(** Creates [Data_types.meta_entry] from meta and its package DB rows *)

let module_of_row row opam_row lib_rows =
    let opam_row = List.hd opam_row in 
    let opampath = path_of_opam opam_row#opam_name opam_row#opam_version 
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    and libs = 
        List.map 
            (fun row -> 
                let lib_path = path_of_lib row#mdl_lib opam_row#opam_name opam_row#opam_version in
                (row#mdl_lib, lib_path))
            lib_rows
    in
        {path=row#mdl_path;
        name=row#mdl_name; 
        opampath; 
        opam;
        libs}
(** Creates [Data_types.module_entry] from module, its package and its libraries DB rows *)

let sources_of_rows rows : sources =
    List.map (function row -> 
        let namesrc = row#opam_name
        and path = path_of_src row#opam_name row#opam_version
        and opam = name_of_opam row#opam_name row#opam_version
        and opampath = path_of_opam row#opam_name row#opam_version in
        {
            namesrc;
            path;
            opam;
            opampath
        }
    ) rows
(** Creates [Data_types.sources] from DB rows *)

module Cond = struct 

    type opam_cond = string
    (** Condition that describes 'in package' property *)

    type mdl_cond = string * string
    (** Condition that describes 'in module' property *)

    let make_opam_cond opam = opam
    (** Creates condition from package name (with version) *)

    let make_mdl_cond mdl opam = (mdl,opam)
    (** Creates condition from module name and its package name (with version) *)  

    let eq_pack p1 p2 = String.equal p1 p2
    (** Says if two opam conditions are equal *)

    let eq_mdl (mdl1,opam1) (mdl2,opam2) =
        String.equal mdl1 mdl2
        && String.equal opam1 opam2
    (** Says if two module conditions are equal *)

    let mem_pack p = List.exists (eq_pack p)
    (** Says if opam condition appears in the list *)

    let mem_mdl m = List.exists (eq_mdl m)
    (** Says if module condition appears in the list *)

    let opam_union packs1 packs2 =
        let union = ref packs1 in 
        List.iter (fun pack2 ->
            if not @@ mem_pack pack2 packs1
            then union := pack2::!union
            )
            packs2;
        !union
    (** Makes union of two opam condition lists *)

    let mdl_union mdls1 mdls2 =
        let union = ref mdls1 in 
        List.iter (fun mdl2 ->
            if not @@ mem_mdl mdl2 mdls1
            then union := mdl2::!union
            )
            mdls2;
        !union
    (** Makes union of two module condition lists *)

    let check_validity opam_conds mdl_conds =
        List.for_all (fun (mdl,opam) ->
                if opam_conds <> []
                then mem_pack (make_opam_cond opam) opam_conds
                else true
            )
            mdl_conds
    (** Says if module condition list is valid versus against condition list.  
        It's means that every module's package appears in the opam condition list *)

    let respect_opam_conditions opam opam_conds =
        match opam_conds with
        | [] -> true
        | _ -> mem_pack opam opam_conds
    (** Says if package respects opam conditions *)

    let respect_mdl_conditions mdl mdl_conds =
        match mdl_conds with
        | [] -> true
        | _ -> mem_mdl mdl mdl_conds
    (** Says if module respects module conditions *)
end
(** Module [Cond] that regroups functions dealing with conditions send with [getElements] request.
    Actially there are only two conditions : one that describes either an element belongs to module 
    with specified name, another that describes either an element belongs to specific package. *)

let val_of_row_opt 
    ((in_packs : Cond.opam_cond list), (in_mdls : Cond.mdl_cond list)) 
    row opam_row mdl_row = 
    let open Cond in
    let opam_row = List.hd opam_row 
    and mdl_row = List.hd mdl_row in 
    let opampath = path_of_opam opam_row#opam_name opam_row#opam_version
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version 
    and mdlpath = mdl_row#mdl_path
    and mdl = mdl_row#mdl_name in
    (* If val resepcts one of opam and mdl condition *)
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
(** [val_of_row_opt (in_packs,in_mdls) row opam_row mdl_row] returns Some of [Data_types.val_element] if value and 
    its package and module DB rows respects one of condition in [in_packs] and [in_mdls] lists.
    Otherwise returns None. *)

let count_from_row = function [ Some v ] -> Int64.to_int v | _ -> 0
(** Extracts result of command 'count' from DB row *)

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
(** [count_elements_in_rows (in_packs, in_mdls) opam_row mdl_row row cpt] increments counter [cpt] if value and 
    its package and module DB rows respects one of condition in [in_packs] and [in_mdls] lists. 
    Otherwise returns [cpt] *)
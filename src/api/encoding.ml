open Json_encoding
open Data_types

let version = conv
  (fun {v_db; v_db_version} -> (v_db, v_db_version))
  (fun (v_db, v_db_version) -> {v_db; v_db_version}) @@
  obj2
    (req "db" string)
    (req "db_version" int)

let api_config = obj1 (opt "port" int)

let info_encoding = conv
    (fun {www_apis} -> www_apis)
    (fun www_apis -> {www_apis}) @@
  obj1
    (req "apis" (list string))

type opam_entry = Data_types.opam_entry = {
  opam_name : string;
  opam_path : string;
  opam_version : string;
  opam_synopsis : string;
} [@@deriving json_encoding]
    
let packages = list opam_entry_enc

type lib_entry = Data_types.lib_entry = {
  lib_name : string;
  lib_path : string;
  lib_opam : string ;
  lib_opam_path : string;
} [@@deriving json_encoding]

let libraries = list lib_entry_enc

type meta_entry = Data_types.meta_entry = {
  meta_name : string ;
  meta_path : string ;
  meta_opam : string ;
  meta_opam_path : string ;
} [@@deriving json_encoding]

let metas = list meta_entry_enc

type module_entry = Data_types.module_entry = {
  mdl_name : string;
  mdl_path : string;
  mdl_opam : string;
  mdl_opam_path: string;
  mdl_libs : (string * string) list;
} [@@deriving json_encoding]

let modules = list module_entry_enc 

type source_entry = Data_types.source_entry = {
  src_name : string;
  src_path : string;
  src_opam : string;
  src_opam_path : string;
} [@@deriving json_encoding]

let sources = list source_entry_enc

type command_result = Data_types.command_result = {
  result : string;
} [@@deriving json_encoding]
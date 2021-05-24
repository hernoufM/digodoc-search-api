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

type module_entry = Data_types.module_entry = {
  mdl_id : int32;
  search_id : string;
  html_path : string;
  mdl_name : string;
  opam_package : string;
  mdl_opam_name : string;
  mdl_opam_version : string;
  libs : (string * string) list;
} [@@deriving json_encoding]

let modules = list module_entry_enc
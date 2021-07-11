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

type nonrec opam_entry = Data_types.opam_entry = {
  name : string;
  path : string;
  version : string;
  synopsis : string;
} [@@deriving json_encoding]
    
let packages = list opam_entry_enc

type nonrec lib_entry = Data_types.lib_entry = {
  name : string;
  path : string;
  opam : string ;
  opampath : string;
} [@@deriving json_encoding]

let libraries = list lib_entry_enc

type nonrec meta_entry = Data_types.meta_entry = {
  name : string ;
  path : string ;
  opam : string ;
  opampath : string ;
} [@@deriving json_encoding]

let metas = list meta_entry_enc

type nonrec module_entry = Data_types.module_entry = {
  name : string;
  path : string;
  opam : string;
  opampath: string;
  libs : (string * string) list;
} [@@deriving json_encoding]

let modules = list module_entry_enc 

type nonrec source_entry = Data_types.source_entry = {
  name : string;
  path : string;
  opam : string;
  opampath : string;
} [@@deriving json_encoding]

let sources = list source_entry_enc

type command_result = Data_types.command_result = {
  result : string;
} [@@deriving json_encoding]

type search_result = Data_types.search_result = {
  packages : opam_entry list;
  libraries : lib_entry list;
  modules : module_entry list;
} [@@deriving json_encoding]
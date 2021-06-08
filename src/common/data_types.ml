type version = {
  v_db: string;
  v_db_version: int;
}

type www_server_info = {
  www_apis : string list;
}

type opam_entry = {
  opam_name : string;
  opam_path : string;
  opam_version : string;
  opam_synopsis : string;
}

type packages = opam_entry list

type lib_entry = {
  lib_name : string;
  lib_path : string;
  lib_opam : string ;
  lib_opam_path : string;
}

type libraries = lib_entry list 

type library_entry = {
  lib_name : string ;
  lib_opam_name : string ;
}

type meta_entry = {
  meta_name : string ;
  meta_path : string ;
  meta_opam : string ;
  meta_opam_path : string ;
}
 
type metas = meta_entry list

type module_entry = {
  mdl_name : string;
  mdl_path : string;
  mdl_opam : string;
  mdl_opam_path: string;
  mdl_libs : (string * string) list;
}

type modules = module_entry list

type source_entry = {
  src_name : string;
  src_path : string;
  src_opam : string;
  src_opam_path : string;
}

type sources = source_entry list


type entry_info = {
  last_id: int64;
  pattern: string;
}


type entry_type = 
  | PACK
  | LIB
  | MOD
  | META
  | SRC
type command =
  | Count of entry_type

type command_result = {
  result : string;
}

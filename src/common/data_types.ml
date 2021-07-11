type version = {
  v_db: string;
  v_db_version: int;
}

type www_server_info = {
  www_apis : string list;
}

type nonrec opam_entry = {
  name : string;
  path : string;
  version : string;
  synopsis : string;
}

type nonrec packages = opam_entry list

type nonrec lib_entry = {
  name : string;
  path : string;
  opam : string ;
  opampath : string;
}

type nonrec libraries = lib_entry list 

type nonrec meta_entry = {
  name : string ;
  path : string ;
  opam : string ;
  opampath : string ;
}
 
type nonrec metas = meta_entry list

type nonrec module_entry = {
  name : string;
  path : string;
  opam : string;
  opampath: string;
  libs : (string * string) list;
}

type nonrec modules = module_entry list

type nonrec source_entry = {
  name : string;
  path : string;
  opam : string;
  opampath : string;
}

type nonrec sources = source_entry list


type entry_info = {
  last_id: int64;
  starts_with : string;
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

type search_result = {
  packages : packages;
  libraries : libraries;
  modules : modules;
}
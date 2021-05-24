type version = {
  v_db: string;
  v_db_version: int;
}

type www_server_info = {
  www_apis : string list;
}

type module_entry = {
  mdl_id : int32;
  search_id : string;
  html_path : string;
  mdl_name : string;
  opam_package : string;
  mdl_opam_name : string;
  mdl_opam_version : string;
  libs : (string * string) list;
}

type modules = module_entry list
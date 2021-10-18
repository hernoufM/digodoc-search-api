
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
(*  *)
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

type entries =
  | Opam of packages
  | Lib of libraries
  | Mdl of modules
  | Meta of metas
  | Src of sources

type entry_type = PACK | LIB | MOD | META | SRC

type pattern = string

type entry_info = {
  mutable entry : entry_type;
  mutable last_id: int;
  mutable starts_with : string;
  mutable pattern: pattern;
}

type nonrec val_element = {
  ident : string;
  value : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
}

type nonrec vals = val_element list

type ocaml_elements =
  | Val of vals

type element_type = VAL

type condition =
  | In_opam of pattern
  | In_mdl of pattern

type search_mode =
  | Regex
  | Text
type element_info = {
  element : element_type;
  last_id : int;
  pattern : pattern;
  mode : search_mode;
  conditions : condition list
}

type info = 
  | Entry of entry_info
  | Element of element_info


type command = Count 

type command_result = {
  result : string;
}

type search_result = {
  packages : packages;
  libraries : libraries;
  modules : modules;
}

type server_error =
  | Invalid_regex 
  | Unknown
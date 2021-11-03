(** Module [Data_types] defines all server-side types also used by user-side application *)

(** {1 Return types} *)

type www_server_info = {
  www_apis : string list;
}
(** Type that lists all server host names *)

type nonrec opam_entry = {
  name : string;
  path : string;
  version : string;
  synopsis : string;
}
(** Opam entry *)

type nonrec packages = opam_entry list
(** List of opam entries *)

type nonrec lib_entry = {
  name : string;
  path : string;
  opam : string ;
  opampath : string;
}
(** Library entry *)

type nonrec libraries = lib_entry list 
(** List of library entries *)

type nonrec meta_entry = {
  namemeta : string ;
  path : string ;
  opam : string ;
  opampath : string ;
}
(** Meta entry *)

type nonrec metas = meta_entry list
(** List of meta entries *)

type nonrec module_entry = {
  name : string;
  path : string;
  opam : string;
  opampath: string;
  libs : (string * string) list;
}
(** Module entry *)

type nonrec modules = module_entry list
(** List of module entries *)

type nonrec source_entry = {
  namesrc : string;
  path : string;
  opam : string;
  opampath : string;
}
(** Source entry *)

type nonrec sources = source_entry list
(** List of source entries *)

type entries =
  | Opam of packages
  | Lib of libraries
  | Mdl of modules
  | Meta of metas
  | Src of sources
(** Type that regroups every entry that is a component of OCaml 
  ecosystem (package, library, module, etc.). Result of [getEntries] service. *)

type nonrec val_element = {
  ident : string;
  value : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
}
(** Value element (OCaml function) *)

type nonrec vals = val_element list
(** List of value elements *)

type ocaml_elements =
  | Val of vals
(** Type that regroups every element that is a component of a 
    module (val, type, class, etc.). Result of [getElements] service *)


type command_result = {
  result : string;
}
(** Result of [exec_command] service *)

type search_result = {
  packages : packages;
  libraries : libraries;
  modules : modules;
}
(** Result of [search] service *)

(** {1 Exceptions} *)

type server_error_type =
  | Invalid_regex 
  | Unknown
(** Possible error types that search-api can raise *)

exception Search_api_error of server_error_type
(** Search api exception. When raised by server code
    ez_api encapsulate it inside [EzReq_lwt_S.KnownError]. *)

let search_api_error typ = Search_api_error typ
(** Encapsulate server_error_type *)

let server_error_type (Search_api_error typ) = typ 
(** Decapsulate server_error_type *)

(** {1 Argument types} *)

type entry_type = PACK | LIB | MOD | META | SRC
(** Entry type *)

type element_type = VAL
(** Element type *)

type pattern = string
(** Search pattern *)

type entry_info = {
  mutable entry : entry_type;
  mutable last_id: int;
  mutable starts_with : string;
  mutable pattern: pattern;
}
(** Entry info, that is used to customise entry search. *)

type condition =
  | In_opam of pattern
  | In_mdl of pattern
(** Condition attached to an OCaml element *)

type search_mode =
  | Regex
  | Text
(** Search mode that determines how to handle correctly an element pattern *)

type element_info = {
  element : element_type;
  last_id : int;
  pattern : pattern;
  mode : search_mode;
  conditions : condition list
}
(** Element info, that is used to customise element search. *)

type info = 
  | Entry of entry_info
  | Element of element_info
(* Type that regroups [entry_info] and [element_info]. 
   Used by services that handle both : entries and elements. *)

type command = Count 
(** All commands that could be executed with service [exec_command]. *)
open Json_encoding
open Data_types


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
  namemeta : string ;
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
  namesrc : string;
  path : string;
  opam : string;
  opampath : string;
} [@@deriving json_encoding]

let sources = list source_entry_enc

let entries =
  let cases =
    [case 
      ~title:"Opam" 
      packages 
      (function | Opam s -> Some s | _ -> None )
      (function s -> Opam s);
    case 
      ~title:"Lib" 
      libraries 
      (function | Lib s -> Some s | _ -> None )
      (function s -> Lib s);
    case 
      ~title:"Mdl" 
      modules
      (function | Mdl s -> Some s | _ -> None )
      (function s -> Mdl s);
    case 
      ~title:"Meta" 
      metas
      (function | Meta s -> Some s | _ -> None )
      (function s -> Meta s);
    case 
      ~title:"Src" 
      sources
      (function | Src s -> Some s | _ -> None )
      (function s -> Src s)
    ]
  in
    union cases 
    
type nonrec val_entry = Data_types.val_element = {
  ident : string;
  value : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
} [@@deriving json_encoding]

let vals = list val_entry_enc 

let ocaml_elements = 
  let cases =
    [case 
      ~title:"Val" 
      vals 
      (function Val s -> Some s)
      (function s -> Val s)
    ]
  in
    union cases
  
type command_result = Data_types.command_result = {
  result : string;
} [@@deriving json_encoding]

type search_result = Data_types.search_result = {
  packages : opam_entry list;
  libraries : lib_entry list;
  modules : module_entry list;
} [@@deriving json_encoding]

let server_error_type = 
  let cases =
    [case 
      ~title:"Invalid_regex" 
       empty
      (function | Invalid_regex -> Some () | _ -> None)
      (function () -> Invalid_regex);
    case 
      ~title:"Unknown" 
       empty
      (function | Unknown -> Some () | _ -> None)
      (function () -> Unknown)
    ]
  in
    union cases 
(** Modules [Encoding] defines JSON encodings for OCaml data types
    used by Digodoc-search API. *)

val api_config : int option Json_encoding.encoding
(** Encoding for 'api_config.json' file. *)

val entries : Data_types.entries Json_encoding.encoding
(** Encoding for [Data_types.entries]. *)

val modules_name : (string * string) list Json_encoding.encoding
(** Encoding for [Data_types.modules]. *)

val ocaml_elements : Data_types.ocaml_elements Json_encoding.encoding
(** Encoding for [Data_types.elements]. *)

val command_result_enc : Data_types.command_result Json_encoding.encoding
(** Encoding for [Data_types.command_result]. *)

val search_result_enc : Data_types.search_result Json_encoding.encoding
(** Encoding for [Data_types.search_result]. *)

val sources_search_result_enc : Data_types.sources_search_result Json_encoding.encoding
(** Encoding for [Data_types.sources_search_result]. *)

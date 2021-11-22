open Data_types 

(** Module [Utils] defines some usefull functions to manipulate data types defined in [Data_types] module. *)

(** Conversion usefull functions *)
module Conv = UtilsEncoding 

let pattern_of_string : string -> pattern = Conv.PathSegment.decode 
(** Pattern decoding *)
    
let pattern_to_string : pattern -> string = Conv.PathSegment.encode
(** Pattern encoding *)

let entry_type_of_string = function 
    | "packages" -> PACK 
    | "libraries" -> LIB 
    | "metas" -> META 
    | "modules" -> MOD 
    | "sources" -> SRC 
    | s -> failwith ("Not valid entry type : " ^ s)  
(** Conversion from [string] to [entry_type]. Raises [Failure] if can't convert to [entry_type]. *)

let entry_type_to_string = function 
    | PACK -> "packages" 
    | LIB -> "libraries"
    | META -> "metas"
    | MOD -> "modules" 
    | SRC -> "sources" 
(** Conversion from [entry_type] to [string] *)

let entry_info_of_string str =
    (* split on separator between path segments *)
    match String.split_on_char '+' str with
    | "entry"::entry::last_id::starts_with::patt::[] ->
        let entry = entry_type_of_string entry
        and last_id = int_of_string last_id
        and pattern = pattern_of_string patt
        and starts_with = Uri.pct_decode starts_with
        in {entry; last_id; starts_with; pattern}
    | _ -> failwith ("Not valid entry info : " ^ str)  
(** Conversion from [string] to [entry_info]. Raises [Failure] if can't convert to [entry_info]. *)

let entry_info_to_string
    { entry; last_id; starts_with; pattern } =
    Printf.sprintf "entry+%s+%d+%s+%s"
        (entry_type_to_string entry)
        last_id
        starts_with
        (pattern_to_string pattern) 
(** Conversion from [entry_info] to [string] *)

let element_type_of_string = function 
    | "vals" -> VAL 
    | s -> failwith ("Not valid element type : " ^ s)  
(** Conversion from [string] to [element_type]. Raises [Failure] if can't convert to [element_type]. *)

let element_type_to_string = function 
    | VAL -> "vals" 
(** Conversion from [element_type] to [string] *)

let element_info_of_string str =
    (* construct list of condition from list of string *)
    let rec conditions_from_list l =
        match l with
        | [] -> []
        | "opam"::pattern::ll -> In_opam (pattern_of_string pattern)::conditions_from_list ll
        | "mdl"::pattern::ll -> In_mdl (pattern_of_string pattern)::conditions_from_list ll
        | _ -> failwith ("Not valid element info : " ^ str)  
    (* search_mode from string *)
    and mode_of_string str =
        match str with
        | "regex" -> Regex
        | "text" -> Text
        | _ -> failwith ("Not valid search mode : " ^ str)
    in
        match String.split_on_char '+' str with
        | "element"::element::last_id::pattern::mode::ll ->
            let element = element_type_of_string element
            and last_id = int_of_string last_id
            and pattern = pattern_of_string pattern 
            and mode = mode_of_string mode 
            and conditions = conditions_from_list ll in
            {element; last_id; pattern; mode; conditions}
        | _ -> failwith ("Not valid element info : " ^ str)  
(** Conversion from [string] to [element_info]. Raises [Failure] if can't convert to [element_info]. *)

let element_info_to_string
    { element; last_id; pattern; mode; conditions } =
    (* converts list of conditions to list of string *)
    let rec conditions_to_string conds =
        match conds with
        | [] -> ""
        | In_opam pattern::ll -> "+opam+"^ pattern_to_string pattern^ (conditions_to_string ll)
        | In_mdl pattern::ll -> "+mdl+"^ pattern_to_string pattern^ (conditions_to_string ll)
    (* search_mode to string *)
    and mode_to_string m =
        match m with
        | Regex -> "regex"
        | Text -> "text"
    in 
        Printf.sprintf "element+%s+%d+%s+%s%s"
            (element_type_to_string element)
            last_id
            (pattern_to_string pattern)
            (mode_to_string mode)     
            (conditions_to_string conditions)
(** Conversion from [element_info] to [string] *)

let info_of_srting str =
    (* get first path segment, that is keyword to separate entry_info and element_info *)
    let ind =  List.nth_opt (String.split_on_char '+' str) 0 in
    match ind with
    | Some "entry" -> Entry (entry_info_of_string str)
    | Some "element" -> Element (element_info_of_string str)
    | _ -> failwith ("Not valid info : " ^ str)  
(** Conversion from [string] to [info]. Raises [Failure] if can't convert to [info]. *)

let info_to_string = function
    | Entry entry -> entry_info_to_string entry
    | Element elt -> element_info_to_string elt
(** Conversion from [info] to [string] *)

let command_of_string = function
    | "count" -> Count
    |  str -> failwith ("Not valid command : " ^ str)  
(** Conversion from [string] to [command]. Raises [Failure] if can't convert to [command]. *)

let command_to_string = function
    | Count -> "count"
(** Conversion from [command] to [string]. *)

let empty_entries entries = 
    match entries with 
    | Opam [] | Lib [] | Mdl [] | Meta [] | Src [] -> true
    | _ -> false
(** Says if [entries] is empty. *)

let empty_elements elements = 
    match elements with 
    | Val [] -> true
    | _ -> false
(** Says if [elements] is empty. *)

let pattern_from_info info =
    match info with
    | Entry entry_info -> entry_info.pattern
    | Element element_info -> element_info.pattern 
(** Get pattern from [info]. *)

let sources_search_info_of_string str =
    match String.split_on_char '+' str with
    | patt :: regex :: case :: last_match_id :: [] ->
        let pattern = pattern_of_string patt
        and is_regex = bool_of_string regex
        and is_case_sensitive = bool_of_string case 
        and last_match_id = int_of_string last_match_id in
        {is_regex; is_case_sensitive; pattern; last_match_id}
    | _ -> failwith ("Not valid sources_search_info info : " ^ str)
(** Conversion from [string] to [sources_search_info]. *)

let sources_search_info_to_string 
    { is_regex; is_case_sensitive; pattern; last_match_id } =
    Printf.sprintf "%s+%b+%b+%d"
        (pattern_to_string pattern)
        is_regex
        is_case_sensitive
        last_match_id
(** Conversion from [sources_search_info] to [string] *)

let to_result : type conv. 
    string ->
    convf:(string -> conv) -> 
    (conv, string) result 
    =
    fun str ~convf ->
        try
            Ok (convf str)
        with 
            Failure str -> Error ("Not recognized data_type : " ^ str)
(** [to_result str ~convf] encapsulates application of [convf] on [str] within [result] type *)
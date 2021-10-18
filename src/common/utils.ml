open Data_types 

module Conv = UtilsConversion 

let pattern_of_string : ?uri:bool-> string -> pattern =
    fun ?(uri=false) str ->
        let str = 
            if uri
            then Uri.pct_decode str
            else str
        in
            let pattern = Conv.PathSegment.decode str in
            if pattern = "~empty~" 
            then "" 
            else pattern
    
let pattern_to_string : pattern -> string = 
    fun pattern ->
        let pattern =
            if pattern = ""
            then "~empty~"
            else pattern
        in
            Conv.PathSegment.encode pattern
            |> Uri.pct_encode


let entry_type_of_string = function 
    | "packages" -> PACK 
    | "libraries" -> LIB 
    | "metas" -> META 
    | "modules" -> MOD 
    | "sources" -> SRC 
    | s -> failwith ("Not valid entry type : " ^ s)  

let entry_type_to_string = function 
    | PACK -> "packages" 
    | LIB -> "libraries"
    | META -> "metas"
    | MOD -> "modules" 
    | SRC -> "sources" 


let entry_info_of_string str =
    match String.split_on_char '+' str with
    | "entry"::entry::last_id::starts_with::pattern::[] ->
        let entry = entry_type_of_string entry
        and last_id = int_of_string last_id
        and starts_with = "^" ^ starts_with 
        and pattern = pattern_of_string pattern
        in
            {entry; last_id; starts_with; pattern}
    | _ -> failwith ("Not valid entry info : " ^ str)  

let entry_info_to_string
    { entry; last_id; starts_with; pattern } =
    Printf.sprintf "entry+%s+%d+%s+%s"
        (entry_type_to_string entry)
        last_id
        (String.make 1 starts_with.[1])
        (pattern_to_string pattern)    

let element_type_of_string = function 
    | "vals" -> VAL 
    | s -> failwith ("Not valid element type : " ^ s)  

let element_type_to_string = function 
    | VAL -> "vals" 

let element_info_of_string str =
    let rec conditions_from_list l =
        match l with
        | [] -> []
        | "opam"::pattern::ll -> In_opam (pattern_of_string pattern)::conditions_from_list ll
        | "mdl"::pattern::ll -> In_mdl (pattern_of_string pattern)::conditions_from_list ll
        | _ -> failwith ("Not valid element info : " ^ str)  
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

let element_info_to_string
    { element; last_id; pattern; mode; conditions } =
    let rec conditions_to_string conds =
        match conds with
        | [] -> ""
        | In_opam pattern::ll -> "+opam+"^ pattern_to_string pattern^ (conditions_to_string ll)
        | In_mdl pattern::ll -> "+mdl+"^ pattern_to_string pattern^ (conditions_to_string ll)
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

let info_of_srting str =
    let ind =  List.nth_opt (String.split_on_char '+' str) 0 in
    match ind with
    | Some "entry" -> Entry (entry_info_of_string str)
    | Some "element" -> Element (element_info_of_string str)
    | _ -> failwith ("Not valid info : " ^ str)  

let info_to_string = function
    | Entry entry -> entry_info_to_string entry
    | Element elt -> element_info_to_string elt

let command_of_string = function
    | "count" -> Count
    |  str -> failwith ("Not valid command : " ^ str)  

let command_to_string = function
    | Count -> "count"

let empty_entries entries = 
    match entries with 
    | Opam [] | Lib [] | Mdl [] | Meta [] | Src [] -> true
    | _ -> false
    
let empty_elements elements = 
    match elements with 
    | Val [] -> true
    | _ -> false

let pattern_from_info info =
    match info with
    | Entry entry_info -> entry_info.pattern
    | Element element_info -> element_info.pattern 
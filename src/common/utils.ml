open Data_types 

let entry_info_of_string = function 
    | "packages" -> PACK 
    | "libraries" -> LIB 
    | "metas" -> META 
    | "modules" -> MOD 
    | "sources" -> SRC 
    | s -> failwith ("Not valid entry type : " ^ s)  

let entry_info_to_string = function 
    | PACK -> "packages" 
    | LIB -> "libraries"
    | META -> "metas"
    | MOD -> "modules" 
    | SRC -> "sources" 

let command_of_string str =
    let open List in 
    let command,entry= 
        let l = String.split_on_char '+' str in
        hd l, tl l |> hd
    in
        let e = entry_info_of_string entry in 
        match command with
        | "count" -> Count e
        | s -> failwith ("Not valid entry type : " ^ s)  

let command_to_string command =
    match command with
    | Count e -> Printf.sprintf "count+%s" (entry_info_to_string e)

let adjust_entry_info {last_id;starts_with;pattern} = 
    let starts_with = "^" ^ starts_with 
    and pattern = if pattern = "~" then "" else pattern in
    {last_id; starts_with; pattern}

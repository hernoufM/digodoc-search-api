
module PathSegment = struct 
    open Ez_subst.V1

    let sep = '+'
    let sep_code = "plus"
    let sep_var = "${plus}"

    let decode ?(uri=false) seg =
        let rec decode_subst () = function 
        | var_code when var_code = sep_code -> String.make 1 sep
        | s ->  failwith ("Ez_subst : Not recognised variable : " ^ s)
    in
        let seg = if uri then Uri.pct_decode seg else seg in
        EZ_SUBST.string seg ~brace:decode_subst ~ctxt:()
    
    let encode str =
        let seg = Buffer.create 13 in
        String.iter (function 
            | ch when ch=sep ->
                Buffer.add_string seg sep_var
            | ch ->
                Buffer.add_char seg ch
            )
            str;
        Buffer.contents seg |> Uri.pct_encode 
end
    

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

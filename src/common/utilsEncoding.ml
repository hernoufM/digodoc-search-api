module PathSegment = struct 
    open Ez_subst.V1

    let sep = '+'
    (** Special character used to separe different path segments. Pattern is encoded in order to
    remplace this character. *)

    let sep_code = "plus"
    (** Name of variable for special character substituation. *)

    let sep_var = "${" ^ sep_code ^ "}"
    (** Variable for special character substituation. *)

    let decode ?(uri=false) seg =
        (* Decode a variable when found *)
        let rec decode_subst () = function 
        | var_code when var_code = sep_code -> String.make 1 sep
        | s ->  failwith ("Ez_subst : Not recognised variable : " ^ s)
        in
            (* if uri is true then firstly decode segment with Uri module *)
            let seg = if uri then Uri.pct_decode seg else seg in
            (* Makes substituations *)
            EZ_SUBST.string seg ~brace:decode_subst ~ctxt:()
    (** [decode ?uri seg] decodes path segment. If [uri] is [true] then decodes [seg] firstly with Uri module.
        Raises [Failure] if segment is not correctly encoded. *)

    let encode pattern =
        let seg = Buffer.create 13 in
        String.iter (function 
            | ch when ch=sep ->
                (* remplaces separator with ez_subst variable *)
                Buffer.add_string seg sep_var
            | ch ->
                Buffer.add_char seg ch
            )
            pattern;
        Buffer.contents seg |> Uri.pct_encode
    (** [encode pattern] encodes pattern so it can be used as a path segement. Also encodes with Uri module. *)
end
(** Module [PathSegment] defines encoding for path segment to call specific service. *)

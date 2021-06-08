open Data_types
open Encoding
open EzAPI

let section_main = Doc.section "API"
let sections = [ section_main ]


module Args = struct
  let entry_info = 
    Arg.make
      ~example:{last_id=Int64.of_int 0;pattern="~"} 
      ~descr:"Entry info for Search API"
      ~name:"entry_info" 
      ~destruct:(fun str ->
        match String.split_on_char '+' str with
        | last_id :: pattern::_ -> Ok {last_id=Int64.of_string last_id; pattern}
        | _ -> Error "Not recognized data_type : str"
      )
      ~construct:(fun {last_id;pattern} ->
        Printf.sprintf "%d+%s" (Int64.to_int last_id) pattern
      )
      ()

  let command = 
    Arg.make
      ~example:(Count META)
      ~descr:"Name of command to execute by Search API"
      ~name:"command" 
      ~destruct:(fun str -> 
          try
            Ok (Utils.command_of_string str)
          with Failure s -> Error s)
      ~construct:Utils.command_to_string
      ()
  (*
  let last_id = Arg.Arg.int ~example:1 ~descr:"Last ID of module" "last_id"
  let pattern = Arg.string ~example:"toto" ~descr:"Search pattern" "pattern"
  let command = Arg.string ~example:"count" ~descr:"API Command" "command"
  let entry = Arg.string  ~example:"modules" ~descr:"Name of entry" "entry"*)
end

let version : (version, exn, no_security) service0 =
  service
    ~section:section_main
    ~name:"version"
    ~output:version
    Path.(root // "version")

let package_entries : (entry_info,packages,exn,no_security) service1 = 
  service
    ~section:section_main
    ~name:"packages"
    ~descr:"Packages"
    ~output:packages
    Path.(root // "packages" /: Args.entry_info)

let library_entries : (entry_info,libraries,exn,no_security) service1 = 
  service
    ~section:section_main
    ~name:"libraries"
    ~descr:"Libraries"
    ~output:libraries
    Path.(root // "libraries" /: Args.entry_info)

let meta_entries : (entry_info,metas,exn,no_security) service1 = 
  service
    ~section:section_main
    ~name:"metas"
    ~descr:"Metas"
    ~output:metas
    Path.(root // "metas" /: Args.entry_info)


let module_entries : (entry_info,modules,exn,no_security) service1 = 
  service
    ~section:section_main
    ~name:"modules"
    ~descr:"Modules"
    ~output:modules
    Path.(root // "modules" /: Args.entry_info)


let source_entries : (entry_info,sources,exn,no_security) service1 = 
  service
    ~section:section_main
    ~name:"sources"
    ~descr:"sources"
    ~output:sources
    Path.(root // "sources" /: Args.entry_info)

let exec_command : (command, entry_info, command_result, exn, no_security) service2 = 
  service
    ~section:section_main
    ~name:"command"
    ~descr:"Execute command with given entry info"
    ~output:command_result_enc
    Path.(root // "command" /: Args.command /: Args.entry_info)
open Data_types
open Encoding
open EzAPI

let section_main = Doc.section "API"
let sections = [ section_main ]

module Args = struct
  open Utils

  let entry_info_ex =  {
    entry=PACK; 
    last_id=0; 
    starts_with="^."; 
    pattern="zarith"
  } 

  let element_info_ex = {
    element=VAL;
    last_id=0;
    pattern="to_str.ng"; 
    mode=Regex; 
    conditions=[In_opam "zarith"; In_mdl "Z"]
  }

  let pattern = Arg.string ~descr:"Pattern of entry" ~example:"zarith" "pattern"   
  
  let entry_info = 
    Arg.make
      ~example: entry_info_ex
      ~descr:"Entry info for Search API"
      ~name:"entry_info" 
      ~destruct:(to_result ~convf:entry_info_of_string)
      ~construct:entry_info_to_string
      ()

  let element_info =
    Arg.make
      ~example:element_info_ex
      ~descr:"Element info for Search API"
      ~name:"element_info" 
      ~destruct:(to_result ~convf:element_info_of_string)
      ~construct:element_info_to_string
      ()

  let command = 
    Arg.make
      ~example:Count
      ~descr:"Name of command to execute by Search API"
      ~name:"command" 
      ~destruct:(to_result ~convf:command_of_string)
      ~construct:command_to_string
      ()

  let info =
    Arg.make
      ~example:(Element element_info_ex)
      ~descr:"Information about data used for global commands"
      ~name:"info" 
      ~destruct:(to_result ~convf:info_of_srting)
      ~construct:info_to_string
      ()
  
end

let entries : (entry_info, entries, server_error, no_security) service1 =
  service
    ~section:section_main
    ~name:"entries"
    ~descr:"Get entries basing on information specified in argument"
    ~output:entries
    Path.(root // "entries" /: Args.entry_info)

let elements : (element_info,ocaml_elements,server_error,no_security) service1 = 
  service
    ~section:section_main
    ~name:"elements"
    ~descr:"Get ocaml elements basing on information specified in argument"
    ~output:ocaml_elements
    Path.(root // "elements" /: Args.element_info)

let exec_command : (command, info, command_result, server_error, no_security) service2 = 
  service
    ~section:section_main
    ~name:"command"
    ~descr:"Execute command with given info in arguments"
    ~output:command_result_enc
    Path.(root // "command" /: Args.command /: Args.info)

let search : (pattern,search_result,server_error,no_security) service1 =
  service
    ~section:section_main
    ~name:"search"
    ~descr:"Search entries (only 10 results returned)"
    ~output:search_result_enc
    Path.(root // "search" /: Args.pattern)
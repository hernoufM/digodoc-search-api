open Data_types
open Encoding
open EzAPI

(** Module [Services] defines all the services with their arguments, results and errors JSON encodings. *)

(** {1 Sections} *)

let section_entries = Doc.section "Entries"

let section_elements = Doc.section "Elements"

let section_commands = Doc.section "Commands"

let section_search = Doc.section "Search"

let sections = [ section_entries; section_elements; section_commands; section_search ]
(** All documentation sections. Every service should be associated to one of them in order to appear in API documentation. *)

(** {1 Service arguments} *)

module Args = struct
  
  open Utils

  let entry_info_ex =  {
    entry=PACK; 
    last_id=0; 
    starts_with="^."; 
    pattern="zarith"
  } 
  (** Example of [Data_types.entry_info] *)

  let element_info_ex = {
    element=VAL;
    last_id=0;
    pattern="to_str.ng"; 
    mode=Regex; 
    conditions=[In_opam "zarith"; In_mdl "Z"]
  }
  (** Example of [Data_types.element] *)

  let sources_search_info_ex = {
    is_regex=true;
    is_case_sensitive=true;
    pattern="it.r";
    last_match_id=10
  }

  let pattern = Arg.string ~descr:"Pattern of entry" ~example:"zarith" "pattern"   
  (** [Data_types.pattern] argument *)

  let entry_info = 
    Arg.make
      ~example: entry_info_ex
      ~descr:"Information about entry used to clarify result."
      ~name:"entry_info" 
      ~destruct:(to_result ~convf:entry_info_of_string)
      ~construct:entry_info_to_string
      ()
  (** [Data_types.entry_info] argument *)

  let element_info =
    Arg.make
      ~example:element_info_ex
      ~descr:"Information about element used to clarify result."
      ~name:"element_info" 
      ~destruct:(to_result ~convf:element_info_of_string)
      ~construct:element_info_to_string
      ()
  (** [Data_types.element_info] argument *)

  let command = 
    Arg.make
      ~example:Count
      ~descr:"Name of command to execute by API"
      ~name:"command" 
      ~destruct:(to_result ~convf:command_of_string)
      ~construct:command_to_string
      ()
  (** [Data_types.command] argument *)

  let info =
    Arg.make
      ~example:(Element element_info_ex)
      ~descr:"Information about data used to clarify result."
      ~name:"info" 
      ~destruct:(to_result ~convf:info_of_srting)
      ~construct:info_to_string
      ()
  (** [Data_types.info] argument *)

  let sources_search_info =
    Arg.make
      ~example:sources_search_info_ex
      ~descr:"Information about fulltext search"
      ~name:"sources_search_info"
      ~destruct:(to_result ~convf:sources_search_info_of_string)
      ~construct:sources_search_info_to_string
      ()      
  (** [Data_types.sources_search_info] argument *)
end
(** Module that unions all service arguments. Argument is created
    with [EzAPI.Arg] module*)

(** {1 Service errors} *)

module Errors = struct

  let server_errors = [
    (* Invalid regex error *)
    Err.make
      ~code:500
      ~name:"Invalid_regex"
      ~encoding:Json_encoding.unit
      ~select:(function Invalid_regex -> Some () | _ -> None)
      ~deselect:(fun () -> Invalid_regex);
    (* Load config for sources DB error *)
    Err.make
      ~code:500
      ~name:"No_sources_config"
      ~encoding:Json_encoding.unit
      ~select:(function No_sources_config -> Some () | _ -> None)
      ~deselect:(fun () -> No_sources_config);
    (* Unknown error *)
    Err.make
      ~code:500
      ~name:"Unknown"
      ~encoding:Json_encoding.unit
      ~select:(function Unknown -> Some () | _ -> None)
      ~deselect:(fun () -> Unknown)
    ]
  (** Table of possible errors returned by service. *)
end 
(** Server errors *)

(** {1 Services} *)

let entries : (entry_info, entries, server_error_type, no_security) service1 =
  service
    ~section:section_entries
    ~name:"entries"
    ~descr:"Get entries basing on information specified in argument"
    ~output:entries
    ~errors:Errors.server_errors
    Path.(root // "entries" /: Args.entry_info)
(** Service that takes as argument [Data_types.entry_info] and returns 50 first results that fits to conditions specified in [entry_info].
    Field [entry] precise type of entry (one of: packages, modules, libraries,...) to search. Field [pattern] is considered as case insensetive 
    regex expression. Field [starts_with] contains another regex pattern that starts with "^" and precises the first letter of an entry. Field
    [last_id] precise previus index of the first entry from which it takes results. Could raise [Search_api_error Invalid_index] if [pattern] 
    isn't a correct regex *)

let elements : (element_info,ocaml_elements,server_error_type,no_security) service1 = 
  service
    ~section:section_elements
    ~name:"elements"
    ~descr:"Get ocaml elements basing on information specified in argument"
    ~output:ocaml_elements
    ~errors:Errors.server_errors
    Path.(root // "elements" /: Args.element_info)
(** Service that takes as argument [Data_types.element_info] and returns 50 first results that fits to conditions specified in [element_info].
    Field [element] precise type of element (one of: types, values, classes ...) to search. Field [pattern] is considered as case insensetive 
    regex expression if field [mode] is set to Regex. Otherwise it is *TODO: describe parsing of pattern in custom mode* . Field [conditions]
    lists all opam packages and modules where element search should take a place. Field [last_id] precise previus index of the first element from 
    which it takes results. Could raise [Search_api_error Invalid_index] if [mode] is regex and [pattern] isn't a valid regex or if one of name
    of one of conditions isn't a correct regex. *)

let exec_command : (command, info, command_result, server_error_type, no_security) service2 = 
  service
    ~section:section_commands
    ~name:"command"
    ~descr:"Execute command with given info in arguments"
    ~output:command_result_enc
    ~errors:Errors.server_errors
    Path.(root // "command" /: Args.command /: Args.info)
(** Service that executes one of predefined command which takes as argument [Data_types.info] that represents an information
    either about entry either about an element. Returns a [Data_types.command_result] representing result of the command. Could raise [Search_api_error Invalid_index] if [pattern] 
    isn't a correct regex. *)

let search : (pattern, search_result, server_error_type, no_security) service1 =
  service
    ~section:section_search
    ~name:"search"
    ~descr:"Search entries (only 10 results returned)"
    ~output:search_result_enc
    ~errors:Errors.server_errors
    Path.(root // "search" /: Args.pattern)
(** Service that takes a regex pattern in argument and returns [Data_types.search_result] that constains at most 10 entries.
    At contains at most 3 packages, at most 3 libraries and the rest of modules. Could raise [Search_api_error Invalid_index] if 
    [pattern] isn't a correct regex *)
  
let search_sources : (sources_search_info, sources_search_result, server_error_type, no_security) service1 =
  service
    ~section:section_search
    ~name:"search_sources"
    ~descr:"Fulltext search within package sources files"
    ~output:sources_search_result_enc
    ~errors:Errors.server_errors
    Path.(root // "sources_search" /: Args.sources_search_info)
(** Service that takes an information about fulltext search [Data_types.sources_search_info]. 
    Field [pattern] describes a substring/regex. Fields [is_regex] and [is_case_sensitive] 
    customise the search. Field [last_match_id] allows to know from which match occurence it should
    server should start to return the results. Service looks up inside [Api.sourcs_db] in order to 
    find a part (limited to 20) of matches inside the lines of sources files for all the packages.*)
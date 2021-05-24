open Data_types
open Encoding
open EzAPI

let section_main = Doc.section "API"
let sections = [ section_main ]


module Args = struct
  let last_id = Arg.int ~example:1 ~descr:"Last ID of module" "last_id"
  let pattern = Arg.string ~example:"toto" ~descr:"Search pattern" "pattern"
end

let version : (version, exn, no_security) service0 =
  service
    ~section:section_main
    ~name:"version"
    ~output:version
    Path.(root // "version")

let module_entry : (int,string,modules,exn,no_security) service2 = 
  service
    ~section:section_main
    ~name:"modules"
    ~descr:"Modules"
    ~output:modules
    Path.(root // "modules" /: Args.last_id /: Args.pattern)

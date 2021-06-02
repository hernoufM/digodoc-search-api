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

let package_entries : (int,string,packages,exn,no_security) service2 = 
  service
    ~section:section_main
    ~name:"packages"
    ~descr:"Packages"
    ~output:packages
    Path.(root // "packages" /: Args.last_id /: Args.pattern)

let library_entries : (int,string,libraries,exn,no_security) service2 = 
  service
    ~section:section_main
    ~name:"libraries"
    ~descr:"Libraries"
    ~output:libraries
    Path.(root // "libraries" /: Args.last_id /: Args.pattern)

let meta_entries : (int,string,metas,exn,no_security) service2 = 
  service
    ~section:section_main
    ~name:"metas"
    ~descr:"Metas"
    ~output:metas
    Path.(root // "metas" /: Args.last_id /: Args.pattern)


let module_entries : (int,string,modules,exn,no_security) service2 = 
  service
    ~section:section_main
    ~name:"modules"
    ~descr:"Modules"
    ~output:modules
    Path.(root // "modules" /: Args.last_id /: Args.pattern)


let source_entries : (int,string,sources,exn,no_security) service2 = 
  service
    ~section:section_main
    ~name:"sources"
    ~descr:"sources"
    ~output:sources
    Path.(root // "sources" /: Args.last_id /: Args.pattern)
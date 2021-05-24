let cver = ref 0
let upgrades : (int * (unit PGOCaml.t -> int -> unit)) list ref = ref []
let downgrades: (int * string list) list ref = ref []

let register_version ?version
    ?(before_upgrade = fun _ -> ())
    ?(after_upgrade = fun _ -> ())
    ~downgrade
    ~upgrade () =
  let previous_version = !cver in
  let version = match version with
    | None -> !cver + 1
    | Some v ->
      if v <= !cver then
        Format.ksprintf failwith "Registering version %d forbidden (min %d)"
          v (!cver + 1);
      v in
  let upgrade dbh version =
    before_upgrade dbh;
    EzPG.upgrade ~dbh ~version ~downgrade upgrade;
    after_upgrade dbh;
  in
  cver := version;
  upgrades := !upgrades @ [previous_version, upgrade];
  downgrades := (version, downgrade) :: !downgrades;
  ()
;;

let init () =

  register_version ()
    ~upgrade:[

      {|create table module_index(
      mdl_id int primary key,
      search_id varchar not null,
      html_path varchar not null,
      mdl_name varchar not null,
      opam_package varchar not null,
      mdl_opam_name varchar not null,
      mdl_opam_version varchar not null
    )|};

      {|create table module_libraries(
      mdl_id int not null references module_index(mdl_id),
      lib_path varchar not null,
      lib_name varchar not null,
      primary key (mdl_id, lib_name)
    )|};

    ]
    ~downgrade:[
      {|ALTER TABLE module_libraries DROP CONSTRAINT module_libraries_mdl_id_fkey|};
      {|drop table module_index|};
      {|drop table module_libraries|};
    ]
;;

let () = init ()

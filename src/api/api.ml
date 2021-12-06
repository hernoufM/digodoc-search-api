open EzAPIServerUtils

(** Module that register all API services and associates to them a handler  *)

module MakeRegisterer(S: module type of Services)(H:module type of Handlers) = struct

  let register s h dir =
    let h a _ b = h a b in
    register s h dir

  let register dir =
    (* Save all services and associated to them handlers *)
    dir
    |> register S.entries H.entries
    |> register S.opam_modules H.opam_modules
    |> register S.elements H.elements
    |> register S.exec_command H.exec_command
    |> register S.search H.search
    |> register S.search_sources H.search_sources

end

module R = MakeRegisterer(Services)(Handlers)

let services =
  empty |> R.register

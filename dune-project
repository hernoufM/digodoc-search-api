(lang dune 2.4)
(name digodoc-search-api)

(source (github OCamlPro/digodoc-search-api))

; TODO : ocamlpro licence
(license MIT)

(authors "Mohamed Hernouf")
(maintainers "mohamed.hernouf@ocamlpro.com")
(homepage https://github.com/OCamlPro/digodoc-search-api)
(bug_reports https://github.com/OCamlPro/digodoc-search-api/issues)

(generate_opam_files true)

(package
 (name digodoc-search-api)
 (synopsis "Search API server for digodoc DB")
 (depends
  ez_api
  ez_file
  ez_search
  odoc
  merlin
  dune
  calendar
  lwt_ppx
  cohttp-lwt-unix
  websocket-lwt-unix
  tls
  cohttp-lwt-jsoo
  re
  ez_subst
  (pgocaml_ppx ( >= 4.3.0))
  ez_pgocaml
  (ppx_deriving_encoding ( >= 0.2.2 ))
  ))

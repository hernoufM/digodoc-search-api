(lang dune 2.4)
(name search-api)

(source (uri git://gitlab.com/o-labs/skeleton))
(license MIT)
(authors "John Doe")
(maintainers "john.doe@origin-labs.com")
(homepage https://gitlab.com/o-labs/skeleton)
(bug_reports https://gitlab.com/o-labs/skeleton/-/issues)

(generate_opam_files true)

(package
 (name search-api)
 (synopsis "Search engine for digodoc entries")
 (depends
  ez_api
  calendar
  geoip
  cohttp-lwt-unix
  re
  ez_subst
  (pgocaml_ppx ( >= 4.3.0))
  ez_pgocaml
  (ppx_deriving_encoding ( >= 0.2.2 ))
  ))

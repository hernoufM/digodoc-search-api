(lang dune 2.4)
(name digodoc-indexer)

(source (uri git://gitlab.com/o-labs/skeleton))
(license MIT)
(authors "John Doe")
(maintainers "john.doe@origin-labs.com")
(homepage https://gitlab.com/o-labs/skeleton)
(bug_reports https://gitlab.com/o-labs/skeleton/-/issues)

(generate_opam_files true)

(package
 (name digodoc-indexer)
 (synopsis "Indexer for digodoc entries")
 (depends
  vue-jsoo
  ez-api
  calendar
  geoip
  cohttp-lwt-unix
  re
  pgocaml_ppx
  ez-pgocaml))

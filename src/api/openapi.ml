let () =
  let sections = Services.sections in
  let docs = Doc.doc in
  EzOpenAPI.executable ~sections ~docs

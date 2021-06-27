(lang dune 2.7)
(generate_opam_files true)

(name webdriver)
(source (github art-w/ocaml-webdriver))
(license MIT)
(authors "Arthur Wendling")
(maintainers "art.wendling@gmail.com")
(version 0.1)

(package
  (name webdriver)
  (synopsis "Remote control interface to web browsers")
  (depends (ocaml (>= "4.08")) yojson base64))

(package
  (name webdriver_cohttp-lwt-unix)
  (synopsis "Remote control interface to web browsers (using Cohttp_lwt_unix)")
  (depends webdriver cohttp-lwt-unix))

(package
  (name webdriver_cohttp-async)
  (synopsis "Remote control interface to web browsers (using Cohttp_async)")
  (depends webdriver cohttp-async))

open Webdriver_cohttp_lwt_unix
open Infix

let test =
  let* () = goto "https://github.com/art-w/ocaml-webdriver" in
  let* commits =
    find_first
      `xpath
      "//a[@href='/art-w/ocaml-webdriver/commits/master']//strong"
  in
  let* nb = text commits in
  let nb = int_of_string nb in
  Printf.printf "number of commits = %i\n%!" nb ;
  return ()

let host = "http://127.0.0.1:4444"
let () =
  try Lwt_main.run (run ~host Capabilities.firefox_headless test)
  with Webdriver e ->
    Printf.fprintf stderr "[FAIL] Webdriver error: %s\n%!" (Error.to_string e) ;
    Printexc.print_backtrace stderr ;
    Printf.fprintf stderr "\n%!"

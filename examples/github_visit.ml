open Lwt
open Webdriver_cohttp_lwt_unix

let host = "http://127.0.0.1:4444"
let target_url = "https://github.com/art-w/ocaml-webdriver"

let test () =
  Session.make ~host Capabilities.firefox >>= fun (session, _) ->
  goto target_url ~session >>= fun () ->
  Lwt_unix.sleep 1.0 >>= fun () ->
  Session.delete ~session

let () = Lwt_main.run (test ())

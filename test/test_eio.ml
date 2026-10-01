module W = Webdriver_cohttp_eio
module Test = Test_any.Make (W)

let host = "http://localhost:4444/wd/hub"

let run_test ~client (name, test) =
  try W.run ~host Test.capabilities test client ;
      Printf.printf "[OK] %s\n%!" name
  with
  | W.Webdriver e ->
      Printf.fprintf stderr "[FAIL] %s\nWebdriver error: %s\n%!"
        name
        (W.Error.to_string e) ;
      Printexc.print_backtrace stderr ;
      Printf.fprintf stderr "\n%!" ;
      ()
  | e ->
      Printf.fprintf stderr "[FAIL] %s\nException %s\n%!"
        name
        (Printexc.to_string e) ;
      Printexc.print_backtrace stderr ;
      Printf.fprintf stderr "\n%!" ;
      ()

let () =
  Eio_main.run @@ fun env ->
  let client = Cohttp_eio.Client.make ~https:None env#net in
  List.iter (run_test ~client) Test.all

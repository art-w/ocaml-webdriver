module W = Webdriver_cohttp_lwt_unix
module Test = Test_any.Make (W)

let host = "http://localhost:4444/wd/hub"

let run_test (name, run) =
  try Lwt_main.run (W.run ~host W.Capabilities.chrome run) ;
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

let () = List.iter run_test Test.all

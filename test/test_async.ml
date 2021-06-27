module W = Webdriver_cohttp_async
module Test = Test_any.Make (W)

let host = "http://localhost:4444/wd/hub"

let run_test (name, test) =
  try Async.Thread_safe.block_on_async_exn
        (fun () -> W.run ~host W.Capabilities.firefox_headless test) ;
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

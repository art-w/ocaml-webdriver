open Webdriver_cohttp_lwt_unix
open Webdriver_cohttp_lwt_unix.Infix

let sleep dt =
  Printf.printf ".%!" ;
  lift (Lwt_unix.sleep dt)

let rec wait cmd =
  Error.catch (fun () -> cmd)
    ~errors:[`no_such_element]
    (fun _ -> sleep 0.1 >>= fun () -> wait cmd)

let wait_for_error cmd =
  Error.catch
    (fun () ->
      let rec loop () =
        let* _ = cmd in
        let* () = sleep 0.1 in
        loop ()
      in
      loop ())
    ~errors:[`no_such_element]
    (fun _ -> return ())


let rec wait_for_condition fn =
  let* b = fn () in
  if b
  then return ()
  else begin
    let* () = sleep 0.1 in
    wait_for_condition fn
  end

let rec list_iter f = function
  | [] -> return ()
  | x :: xs ->
      let* () = f x in
      list_iter f xs

(*
let accept_cookies =
  let* frame = wait (find_first `tag_name "iframe") in
  let* () = switch_to_frame (`elt frame) in
  let* btn = wait (find_first `css "#introAgreeButton") in
  let* () = click btn in
  let* () = switch_to_frame `top in
  let* () = wait_for_error (find_first `tag_name "iframe") in
  Cookie.all
*)

let accept_cookies =
  let* popup = wait (find_first `css "#xe7COe") in
  let* btn = wait (find_first `css "#L2AGLb") in
  let* () = click btn in
  let* () =
    wait_for_condition
      (fun () ->
        let+ visible = css popup "display" in
        visible = "none")
  in
  Cookie.all

let search query =
  let* input = find_first `css "input[name='q']" in
  let* () = send_keys input (query ^ Key.enter) in
  let* results = wait (find_first `xpath "//div[@id='search']") in
  let* links = find_all ~from:results `tag_name "h3" in
  Printf.printf "found %i results\n%!" (List.length links) ;
  list_iter
    (fun link ->
      let* txt = text link in
      Printf.printf "- %s\n%!" txt ;
      return ())
    links

let search_without_cookies =
  let* () = goto "https://google.com" in
  let* cookies = accept_cookies in
  let* () = search "webdriver" in
  return cookies

let search_with_cookies cookies =
  let* () = goto "https://google.com" in
  let* () = list_iter Cookie.add cookies in
  let* () = goto "https://google.com" in
  search "webdriver"


let host = "http://localhost:4444/wd/hub"
let run cmd = Lwt_main.run (run ~host Capabilities.firefox_headless cmd)

let () =
  Printf.printf "### Accept the cookies\n%!" ;
  let cookies = run search_without_cookies in
  Printf.printf "\n%!" ;
  Printf.printf "### Reuse the %i cookies\n%!" (List.length cookies) ;
  run (search_with_cookies cookies)

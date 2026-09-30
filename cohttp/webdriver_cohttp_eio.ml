module Client = struct
  open Cohttp_eio

  type 'a t = Client.t -> 'a

  let return x _ = x
  let map f m c = f (m c)
  let bind f m c = f (m c) c

  let fail e _ = raise e

  let catch f handle c =
    match f () c with
    | x -> x
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        match handle e with
        | Some k -> k c
        | None -> Printexc.raise_with_backtrace e bt

  let protect ~finally f c =
    let finally () = Eio.Cancel.protect (fun () -> finally () c) in
    match f () c with
    | x -> finally () ; x
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        finally () ;
        Printexc.raise_with_backtrace e bt

  let read_body (_resp, body) =
    Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int

  let get url c =
    Eio.Switch.run @@ fun sw ->
    read_body (Client.get c ~sw (Uri.of_string url))

  let post url body c =
    Eio.Switch.run @@ fun sw ->
    read_body
      (Client.post c ~sw
        ~headers:(Http.Header.init_with "Content-Type" "application/json")
        ~body:(Body.of_string body)
        (Uri.of_string url))

  let delete url c =
    Eio.Switch.run @@ fun sw ->
    read_body (Client.delete c ~sw (Uri.of_string url))
end

module W = Webdriver.Make (Client)
include W

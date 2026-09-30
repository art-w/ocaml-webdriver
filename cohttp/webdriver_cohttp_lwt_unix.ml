module Client = struct
  open Cohttp_lwt_unix

  type 'a t = 'a Lwt.t

  let return = Lwt.return
  let map = Lwt.map
  let bind f m = Lwt.( >>= ) m f
  let ( let* ) m fn = Lwt.( >>= ) m fn

  let fail = Lwt.fail

  let catch f handle =
    Lwt.catch f (fun e ->
      match handle e with
      | Some k -> k
      | None -> Lwt.reraise e)

  let protect ~finally f = Lwt.finalize f finally

  let get url =
    let* _resp, body = Client.get (Uri.of_string url) in
    Cohttp_lwt.Body.to_string body

  let post url body =
    let* _resp, body =
      Client.post
        ~chunked:false
        ~headers:(Cohttp.Header.init_with "Content-Type" "application/json")
        ~body:(Cohttp_lwt__Body.of_string body)
        (Uri.of_string url)
    in
    Cohttp_lwt.Body.to_string body

  let delete url =
    let* _resp, body = Client.delete (Uri.of_string url) in
    Cohttp_lwt.Body.to_string body
end

module W = Webdriver.Make (Client)
include W

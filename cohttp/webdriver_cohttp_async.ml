module Client = struct
  open Async
  open Cohttp_async

  type 'a t = 'a Deferred.t

  let return = Deferred.return
  let map f t = Deferred.map ~f t
  let bind f m = Deferred.( >>= ) m f

  let ( let* ) m fn = Deferred.( >>= ) m fn

  let fail e = raise e
  let catch f handle =
    let* x = Async.try_with f in
    match x with
    | Ok x -> return x
    | Error e -> handle e

  let get url =
    let* _resp, body = Client.get (Uri.of_string url) in
    Cohttp_async.Body.to_string body

  let post url body =
    let* _resp, body =
      Client.post
        ~headers:(Cohttp.Header.init_with "Content-Type" "application/json")
        ~body:(Cohttp_async__Body.of_string body)
        (Uri.of_string url)
    in
    Cohttp_async.Body.to_string body

  let delete url =
    let* _resp, body = Client.delete (Uri.of_string url) in
    Cohttp_async.Body.to_string body
end

module W = Webdriver.Make (Client)
include W

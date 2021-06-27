WebDriver is a [W3C specification] to remote control a web browser. This allow
you to simulate and test user interactions on your website in real life
conditions, with javascript enabled, on as many browsers and operating systems
you can get your hands on.

*[Online Documentation]*

You will need to download and run a WebDriver compatible server of your
favorite browser: [Chrome], [Firefox], [Internet Explorer], [Microsoft Edge],
[Opera], [Safari], and optionally with the help of [Selenium].

As a quick example, the `geckodriver` for Firefox listens on
`http://127.0.0.1:4444` by default:

```shell
$ ./geckodriver -v
...	webdriver::httpapi	DEBUG	Creating routes
...	geckodriver	DEBUG	Listening on 127.0.0.1:4444
```

We can connect to this driver and visit our github url with:

```ocaml
open Webdriver_cohttp_lwt_unix
open Infix

let host = "http://127.0.0.1:4444"

let test =
  let* () = goto "https://github.com/art-w/ocaml-webdriver" in
  let* () = lift (Lwt_unix.sleep 1.0) in
  return ()

let () =
  Lwt_main.run (run ~host Capabilities.firefox test)
```

The same example can also be written as:

```ocaml
open Lwt
open Webdriver_cohttp_lwt_unix

let host = "http://127.0.0.1:4444"

let test () =
  Session.make ~host Capabilities.firefox >>= fun (session, _) ->
  goto "https://github.com/art-w/ocaml-webdriver" ~session >>= fun () ->
  Lwt_unix.sleep 1.0 >>= fun () ->
  Session.delete ~session

let () = Lwt_main.run (test ())
```

See the `examples` and `test` folders for more interactions.

[W3C specification]: https://www.w3.org/TR/webdriver/
[Online Documentation]: https://art-w.github.io/ocaml-webdriver
[Chrome]: https://chromedriver.chromium.org/
[Firefox]: https://github.com/mozilla/geckodriver
[Internet Explorer]: https://github.com/SeleniumHQ/selenium/wiki/InternetExplorerDriver
[Microsoft Edge]: https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/
[Opera]: https://github.com/operasoftware/operachromiumdriver
[Safari]: https://developer.apple.com/documentation/webkit/testing_with_webdriver_in_safari
[Selenium]: https://www.selenium.dev/

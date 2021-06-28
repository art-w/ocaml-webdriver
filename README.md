WebDriver is a [W3C specification] to remote control a web browser. This allow
you to simulate and test user interactions on your website in real life
conditions, with javascript enabled, on as many browsers and operating systems
you can get your hands on.

**[Online Documentation]**

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

We can connect to this driver and visit this github url to fetch the number
of commits with:

```ocaml
module W = Webdriver_cohttp_lwt_unix
open W.Infix

let test =
  let* () = W.goto "https://github.com/art-w/ocaml-webdriver" in
  let* commits =
    W.find_first
      `xpath
      "//a[@href='/art-w/ocaml-webdriver/commits/master']//strong"
  in
  let* nb = W.text commits in
  let nb = int_of_string nb in
  Printf.printf "number of commits = %i\n%!" nb ;
  W.return ()

let host = "http://127.0.0.1:4444"
let () = Lwt_main.run (W.run ~host Capabilities.firefox_headless test)
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

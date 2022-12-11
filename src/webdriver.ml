include Webdriver_sig
module Json = Yojson.Safe

module Make (Client : HTTP_CLIENT) = struct

  type 'a io = 'a Client.t

  type json = Json.t
  let ( .%() ) json key = Json.Util.member key json

  type session = string
  type 'a cmd = session:session -> 'a io
  type elt = string

  exception Webdriver of Webdriver_sig.Error.t

  module Error = struct

    type error = Webdriver_sig.Error.error

    type t = Webdriver_sig.Error.t =
      { error : error
      ; message : string
      ; stacktrace : string
      ; data : json
      }

    let protocol_fail message data =
      let t =
        { error = `ocaml_protocol_failure
        ; message
        ; stacktrace = "wrong assumption in ocaml-webdriver"
        ; data
        }
      in
      raise (Webdriver t)

    let kind_of_string = function
      | "element click intercepted" -> `element_click_intercepted
      | "element not interactable" -> `element_not_interactable
      | "insecure certificate" -> `insecure_certificate
      | "invalid argument" -> `invalid_argument
      | "invalid cookie domain" -> `invalid_cookie_domain
      | "invalid element state" -> `invalid_element_state
      | "invalid selector" -> `invalid_selector
      | "invalid session id" -> `invalid_session_id
      | "javascript error" -> `javascript_error
      | "move target out of bounds" -> `move_target_out_of_bounds
      | "no such alert" -> `no_such_alert
      | "no such cookie" -> `no_such_cookie
      | "no such element" -> `no_such_element
      | "no such frame" -> `no_such_frame
      | "no such window" -> `no_such_window
      | "script timeout" -> `script_timeout
      | "session not created" -> `session_not_created
      | "stale element reference" -> `stale_element_reference
      | "timeout" -> `timeout
      | "unable to set cookie" -> `unable_to_set_cookie
      | "unable to capture screen" -> `unable_to_capture_screen
      | "unexpected alert open" -> `unexpected_alert_open
      | "unknown command" -> `unknown_command
      | "unknown error" -> `unknown_error
      | "unknown method" -> `unknown_method
      | "unsupported operation" -> `unsupported_operation
      | msg -> `unspecified msg

    let string_of_kind = function
      | `element_click_intercepted -> "element click intercepted"
      | `element_not_interactable -> "element not interactable"
      | `insecure_certificate -> "insecure certificate"
      | `invalid_argument -> "invalid argument"
      | `invalid_cookie_domain -> "invalid cookie domain"
      | `invalid_element_state -> "invalid element state"
      | `invalid_selector -> "invalid selector"
      | `invalid_session_id -> "invalid session id"
      | `javascript_error -> "javascript error"
      | `move_target_out_of_bounds -> "move target out of bounds"
      | `no_such_alert -> "no such alert"
      | `no_such_cookie -> "no such cookie"
      | `no_such_element -> "no such element"
      | `no_such_frame -> "no such frame"
      | `no_such_window -> "no such window"
      | `script_timeout -> "script timeout"
      | `session_not_created -> "session not created"
      | `stale_element_reference -> "stale element reference"
      | `timeout -> "timeout"
      | `unable_to_set_cookie -> "unable to set cookie"
      | `unable_to_capture_screen -> "unable to capture screen"
      | `unexpected_alert_open -> "unexpected alert open"
      | `unknown_command -> "unknown command"
      | `unknown_error -> "unknown error"
      | `unknown_method -> "unknown method"
      | `unsupported_operation -> "unsupported operation"
      | `unspecified msg -> "(unspecified) " ^ msg
      | `ocaml_protocol_failure -> "(ocaml-webdriver) procotol failure"

    let to_string e =
      Printf.sprintf "{ error = %S ; message = %S ; data = %s ; _ }"
        (string_of_kind e.error)
        e.message
        (Yojson.Safe.to_string e.data)

    let check json =
      match json.%("error") with
      | `Null -> ()
      | `String error ->
          let e =
            { error = kind_of_string error
            ; message = Json.to_string json.%("message")
            ; stacktrace = Json.to_string json.%("stacktrace")
            ; data = json.%("data")
            }
          in
          raise (Webdriver e)
      | v -> protocol_fail "expected null or string" v

    let is_match e kinds = List.mem e.error kinds

    let catch m ?(errors = []) f = fun ~session ->
      Client.catch
        (fun () -> m () ~session)
        (function Webdriver e when is_match e errors -> f e ~session
                | exn -> Client.fail exn)

    let fail e ~session:_ = Client.fail e
  end

  module J = struct
    let unit = function
      | `Null -> ()
      | json -> Error.protocol_fail "expected null" json
    let bool ?default json = match json, default with
      | `Bool b, _ -> b
      | `Null, Some default -> default
      | _ -> Error.protocol_fail "expected bool" json
    let int = function
      | `Int i -> i
      | json -> Error.protocol_fail "expected int" json
    let float = function
      | `Int i -> float_of_int i
      | `Float i -> i
      | json -> Error.protocol_fail "expected float" json
    let string ?default json = match json, default with
      | `String s, _ -> s
      | `Null, Some default -> default
      | _ -> Error.protocol_fail "expected string" json
    let string_option = function
      | `Null -> None
      | `String s -> Some s
      | json -> Error.protocol_fail "expected null or string" json
    let list = function
      | `List lst -> lst
      | json ->  Error.protocol_fail "expected list" json
    let base64 x = Base64.decode_exn (string x)
  end

  module Capabilities = struct
    type t = json

    let capabilities json = `Assoc ["capabilities", json]
    let first_match browsers = `Assoc [("firstMatch", `List browsers)]
    let browser_name name = `Assoc ["browserName", `String name]

    let chrome = capabilities @@ first_match [browser_name "chrome"]

    let chrome_headless =
        capabilities
        @@ first_match
         [ `Assoc [ "browserName", `String "chrome"
                  ; "goog:chromeOptions",
                    `Assoc [ "args", `List [ `String "--headless"
                                           ; `String "--disable-gpu"
                                           ; `String "--no-sandbox"
                                           ; `String "--disable-dev-shm-usage"
                                           ; `String "--window-size=1920,1080"
                                           ]
                           ]
                  ]
         ]

    let firefox = capabilities @@ first_match [browser_name "firefox"]

    let firefox_headless =
      capabilities
      @@ first_match
       [ `Assoc [ "browserName", `String "firefox"
                ; "moz:firefoxOptions",
                  `Assoc [ "args", `List [ `String "-headless" ] ]
                ]
       ]
  end

  let json_value body =
    match (Json.from_string body).%("value") with
    | (`Assoc _) as result ->
        Error.check result ;
        result
    | result -> result

  let client_value t = Client.map json_value t

  let get ~session path =
    client_value (Client.get (session ^ path))

  let post_raw ~session path body =
    client_value (Client.post (session ^ path) body)

  let post ~session path json =
    let body = match json with
      | `Null -> "{}"
      | json -> Json.to_string json
    in
    post_raw ~session path body

  let delete ~session path =
    client_value (Client.delete (session ^ path))

  module Infix = struct
    let lift t = fun ~session:_ -> t

    let return x = fun ~session:_ -> Client.return x
    let ( let+ ) f g = fun ~session -> Client.map g (f ~session)
    let ( let* ) f g =
      fun ~session -> Client.bind (fun x -> g x ~session) (f ~session)

    let ( >>| ) = ( let+ )
    let ( >>= ) = ( let* )

    let ( |<< ) f m = let+ x = m in f x
    let ( =<< ) f m = let* x = m in f x

    let map = ( |<< )
    let bind = ( =<< )

    let map2 f x y =
      let* x = x in
      let+ y = y in
      f x y

    let ( <*> ) f x =
      let* f = f in
      let+ x = x in
      f x

    let ( and* ) x y =
      let* x = x in
      let+ y = y in
      (x, y)

    let ( and+ ) = ( and* )
  end

  open Infix

  module Session = struct
    let absolute_path path =
      let len = String.length path in
      if len > 0 && path.[len - 1] = '/'
      then path
      else path ^ "/"

    let make ~host capabilities =
      let ( let+ ) m f = Client.map f m in
      let session = absolute_path host in
      let+ json = post ~session "session" capabilities in
      let capabilities = json.%("capabilities") in
      let id = J.string json.%("sessionId") in
      session ^ "session/" ^ id, capabilities

    let delete ~session = Client.map J.unit (delete "" ~session)
  end

  let run ~host capabilities t =
    let ( >>= ) m f = Client.bind f m in
    Session.make ~host capabilities >>= fun (session, _) ->
    Client.catch
      (fun () -> Client.map (fun x -> Ok x) (t ~session))
      (fun e -> Client.return (Error e))
    >>= fun result ->
    Session.delete ~session >>= fun () ->
    match result with
    | Ok x -> Client.return x
    | Error e -> Client.fail e

  let title = J.string |<< get "/title"
  let source = J.string |<< get "/source"
  let print = J.base64 |<< post "/print" `Null

  let execute script =
    let json = `Assoc ["script", `String script ; "args", `List []] in
    post "/execute/sync" json

  let execute_async script =
    let json = `Assoc ["script", `String script ; "args", `List []] in
    post "/execute/async" json

  let current_url = J.string |<< get "/url"
  let goto url = J.unit |<< post "/url" (`Assoc ["url", `String url])
  let back = J.unit |<< post "/back" `Null
  let forward = J.unit |<< post "/forward" `Null
  let refresh = J.unit |<< post "/refresh" `Null

  type rect = { x : float ; y : float ; width : float ; height : float }

  let rect_of_json json =
    { x = J.float json.%("x")
    ; y = J.float json.%("y")
    ; width = J.float json.%("width")
    ; height = J.float json.%("height")
    }

  module Window = struct
    type t = string
    type hint = [`tab | `window]

    let string_of_hint = function
      | `tab -> "tab"
      | `window -> "window"

    let make hint =
      let json = `Assoc ["hint", `String (string_of_hint hint)] in
      let+ json = post "/window/new" json in
      let handle = J.string json.%("handle") in
      let kind = match json.%("kind") with
        | `String "window" -> `window
        | `String "tab" -> `tab
        | json -> `other json
      in
      handle, kind

    let current = J.string |<< get "/window"

    let switch_to window =
      J.unit |<< post "/window" (`Assoc ["handle", `String window])

    let handle_list json = List.map Json.Util.to_string (J.list json)

    let close = handle_list |<< delete "/window"
    let all = handle_list |<< get "/window/handles"

    type rect = { x : int ; y : int ; width : int ; height : int }
    let rect_of_json json =
      { x = J.int json.%("x")
      ; y = J.int json.%("y")
      ; width = J.int json.%("width")
      ; height = J.int json.%("height")
      }
    let json_of_rect r =
      `Assoc [ "x", `Int r.x
             ; "y", `Int r.y
             ; "width", `Int r.width
             ; "height", `Int r.height
             ]

    let get_rect = rect_of_json |<< get "/window/rect"
    let set_rect r = rect_of_json |<< post "/window/rect" (json_of_rect r)

    let maximize = rect_of_json |<< post "/window/maximize" `Null
    let minimize = rect_of_json |<< post "/window/minimize" `Null
    let fullscreen = rect_of_json |<< post "/window/fullscreen" `Null

  end

  module Key = struct
    type t = string

    let arrow_down      = "\\uE015"
    let arrow_left      = "\\uE012"
    let arrow_right     = "\\uE014"
    let arrow_up        = "\\uE013"
    let enter           = "\\uE007"
    let return          = "\\uE006"
    let tab             = "\\uE004"
    let alt             = "\\uE00A"
    let meta            = "\\uE03D"
    let shift           = "\\uE008"
    let control         = "\\uE009"
    let escape          = "\\uE00C"
    let f1              = "\\uE031"
    let f10             = "\\uE03A"
    let f11             = "\\uE03B"
    let f12             = "\\uE03C"
    let f2              = "\\uE032"
    let f3              = "\\uE033"
    let f4              = "\\uE034"
    let f5              = "\\uE035"
    let f6              = "\\uE036"
    let f7              = "\\uE037"
    let f8              = "\\uE038"
    let f9              = "\\uE039"
    let help            = "\\uE002"
    let home            = "\\uE011"
    let end_            = "\\uE010"
    let insert          = "\\uE016"
    let backspace       = "\\uE003"
    let delete          = "\\uE017"
    let cancel          = "\\uE001"
    let clear           = "\\uE005"
    let numpad0         = "\\uE01A"
    let numpad1         = "\\uE01B"
    let numpad2         = "\\uE01C"
    let numpad3         = "\\uE01D"
    let numpad4         = "\\uE01E"
    let numpad5         = "\\uE01F"
    let numpad6         = "\\uE020"
    let numpad7         = "\\uE021"
    let numpad8         = "\\uE022"
    let numpad9         = "\\uE023"
    let numpad_add      = "\\uE025"
    let numpad_comma    = "\\uE026"
    let numpad_decimal  = "\\uE028"
    let numpad_divide   = "\\uE029"
    let numpad_enter    = "\\uE007"
    let numpad_multiply = "\\uE024"
    let numpad_subtract = "\\uE027"
    let page_down       = "\\uE00F"
    let page_up         = "\\uE00E"
    let pause           = "\\uE00B"
    let unidentified    = "\\uE000"
    let zenkaku_hankaku = "\\uE040"

    let re_unicode = Str.regexp "\\\\\\(\\uE0[0-9A-F][0-9A-F]\\)"
    let escape_unicode str =
      Str.global_replace re_unicode "\\1" (Printf.sprintf "%S" str)
  end


  type using =
    [ `css
    | `link_text
    | `partial_link_text
    | `tag_name
    | `xpath
    ]

  let string_of_using = function
    | `css -> "css selector"
    | `link_text -> "link text"
    | `partial_link_text -> "partial link text"
    | `tag_name -> "tag name"
    | `xpath -> "xpath"

  let strategy using selector =
    `Assoc [ "using", `String (string_of_using using)
           ; "value", `String selector
           ]

  let web_element_id = "element-6066-11e4-a52e-4f735466cecf"

  let expect_id = function
    | `Assoc [ key, `String id ] ->
        assert (key = web_element_id) ;
        id
    | json ->
        Error.protocol_fail "expected web element identifier" json

  let from id = "/element/" ^ id

  let from_opt = function
    | None -> ""
    | Some id -> from id

  let find_first ?from using selector =
    let query = strategy using selector in
    expect_id |<< post (from_opt from ^ "/element") query

  let find_all ?from using selector =
    let query = strategy using selector in
    let+ json = post (from_opt from ^ "/elements") query in
    List.map expect_id (J.list json)

  let active = expect_id |<< get "/element/active"

  let is_selected elt = J.bool |<< get (from elt ^ "/selected")
  let is_enabled elt = J.bool |<< get (from elt ^ "/enabled")
  let is_displayed elt = J.bool |<< get (from elt ^ "/displayed")

  let attribute elt attr =
    J.string |<< get (from elt ^ "/attribute/" ^ attr)

  let property elt prop =
    J.string_option |<< get (from elt ^ "/property/" ^ prop)

  let css elt prop =
    J.string |<< get (from elt ^ "/css/" ^ prop)

  let text elt = J.string |<< get (from elt ^ "/text")
  let tag_name elt = J.string |<< get (from elt ^ "/name")
  let rect elt = rect_of_json |<< get (from elt ^ "/rect")

  let aria_role elt = J.string |<< get (from elt ^ "/computedrole")
  let aria_label elt = get (from elt ^ "/computedlabel")

  let submit elt = J.unit |<< post (from elt ^ "/submit") `Null
  let click elt = J.unit |<< post (from elt ^ "/click") `Null
  let clear elt = J.unit |<< post (from elt ^ "/clear") `Null

  let send_keys elt keys =
    let json = `Assoc [ "text", `Stringlit (Key.escape_unicode keys) ] in
    let body = Yojson.Raw.to_string json in
    J.unit |<< post_raw (from elt ^ "/value") body

  let switch_to_frame id =
    let id = match id with
      | `top -> `Null
      | `id i -> `Int i
      | `elt e -> `Assoc [ web_element_id, `String e ]
    in
    let json = `Assoc [ "id", id ] in
    J.unit |<< post "/frame" json

  let switch_to_parent_frame =
    J.unit |<< post "/frame/parent" `Null

  let screenshot ?elt () =
    J.base64 |<< get (from_opt elt ^ "/screenshot")

  type pause =
    [ `noop
    | `pause of int
    ]

  type key =
    [ pause
    | `down of string
    | `up of string
    ]

  type button = int

  type move =
    { move_duration : int
    ; move_origin : [`viewport | `pointer | `elt of elt]
    ; move_x : int
    ; move_y : int
    }

  let absolute ?(duration = 0) (x, y) =
    { move_duration = duration
    ; move_origin = `viewport
    ; move_x = x
    ; move_y = y
    }

  let relative ?(duration = 0) (x, y) =
    { move_duration = duration
    ; move_origin = `pointer
    ; move_x = x
    ; move_y = y
    }

  let center ?(duration = 0) ?(offset = (0, 0)) elt =
    { move_duration = duration
    ; move_origin = `elt elt
    ; move_x = fst offset
    ; move_y = snd offset
    }

  type pointer =
    [ pause
    | `cancel
    | `down of button
    | `up of button
    | `move of move
    ]

  type scroll =
    { scroll_duration : int
    ; scroll_origin : [`viewport | `elt of elt]
    ; scroll_x : int
    ; scroll_y : int
    }

  let scroll_absolute ?(duration = 0) ?(x = 0) ?(y = 0) () =
    { scroll_duration = duration
    ; scroll_origin = `viewport
    ; scroll_x = x
    ; scroll_y = y
    }

  let scroll_to ?(duration = 0) ?(dx = 0) ?(dy = 0) elt =
    { scroll_duration = duration
    ; scroll_origin = `elt elt
    ; scroll_x = dx
    ; scroll_y = dy
    }

  type wheel =
    [ pause
    | `scroll of scroll
    ]

  type 'a kind =
    | Null : [`noop | `pause of int] kind
    | Key : key kind
    | Wheel : wheel kind
    | Pointer : [`mouse | `pen | `touch] -> pointer kind

  type 'a source = string * 'a kind
  type action = Do : 'a source * 'a list -> action

  let str s = `Stringlit (Printf.sprintf "%S" s)
  let int i = `Intlit (Printf.sprintf "%i" i)

  let json_of_pause_tick = function
    | `noop -> `Assoc [ "type", str "pause" ]
    | `pause d -> `Assoc [ "type", str "pause" ; "duration", int d ]

  let json_of_key_tick = function
    | (`noop | `pause _) as pause -> json_of_pause_tick pause
    | `down key ->
        `Assoc [ "type", str "keyDown"
               ; "value", `Stringlit (Key.escape_unicode key)
               ]
    | `up key ->
        `Assoc [ "type", str "keyUp"
               ; "value", `Stringlit (Key.escape_unicode key)
               ]

  let json_of_origin = function
    | `viewport -> str "viewport"
    | `pointer -> str "pointer"
    | `elt id -> `Assoc [web_element_id, str id]

  let string_of_origin = function
    | `viewport -> "viewport"
    | `elt id -> id

  let json_of_pointer_tick = function
    | (`noop | `pause _) as pause -> json_of_pause_tick pause
    | `cancel   -> `Assoc ["type", str "pointerCancel"]
    | `down btn -> `Assoc ["type", str "pointerDown" ; "button", int btn]
    | `up btn   -> `Assoc ["type", str "pointerUp" ; "button", int btn]
    | `move m   ->
        `Assoc [ "type", str "pointerMove"
               ; "duration", int m.move_duration
               ; "origin", json_of_origin m.move_origin
               ; "x", int m.move_x
               ; "y", int m.move_y
               ]

  let json_of_wheel_tick = function
    | (`noop | `pause _) as pause -> json_of_pause_tick pause
    | `scroll m   ->
        `Assoc [ "type", str "scroll"
               ; "duration", int m.scroll_duration
               ; "origin", str (string_of_origin m.scroll_origin)
               ; "x", int m.scroll_x
               ; "y", int m.scroll_y
               ]

  let string_of_pointer_kind = function
    | `mouse -> "mouse"
    | `pen -> "pen"
    | `touch -> "touch"

  let assoc_of_kind
  : type a. a kind -> a list -> (string * Yojson.Raw.t) list
  = fun kind lst -> match kind with
    | Null ->
        [ "type", str "none"
        ; "actions", `List (List.map json_of_pause_tick lst)
        ]
    | Key ->
        [ "type", str "key"
        ; "actions", `List (List.map json_of_key_tick lst)
        ]
    | Wheel ->
        [ "type", str "wheel"
        ; "actions", `List (List.map json_of_wheel_tick lst)
        ]
    | Pointer kind ->
        [ "type", str "pointer"
        ; "pointerType", str (string_of_pointer_kind kind)
        ; "actions", `List (List.map json_of_pointer_tick lst)
        ]

  let json_of_action (Do ((id, kind), lst)) =
    `Assoc (("id", str id) :: assoc_of_kind kind lst)

  let button_left   = 0
  let button_middle = 1
  let button_right  = 2

  let none ?(name = "none") actions = Do ((name, Null), actions)
  let keyboard ?(name = "keyboard") actions = Do ((name, Key), actions)
  let mouse ?(name = "mouse") actions = Do ((name, Pointer `mouse), actions)
  let touch ?(name = "touch0") actions = Do ((name, Pointer `touch), actions)
  let pen ?(name = "pen") actions = Do ((name, Pointer `pen), actions)
  let wheel ?(name = "wheel") actions = Do ((name, Wheel), actions)

  let perform actions =
    let actions = List.map json_of_action actions in
    let json = `Assoc [ "actions", `List actions ] in
    let body = Yojson.Raw.to_string json in
    J.unit |<< post_raw "/actions" body

  let release = J.unit |<< delete "/actions"

  let double_click_action =
    let click = [ `down button_left ; `up button_left ] in
    click @ click

  let double_click elt =
    perform [ mouse (`move (center elt) :: double_click_action) ]

  let sleep duration =
    perform [ none [ `pause duration ] ]

  let key_modifiers = [ Key.control ;  Key.shift ; Key.alt ; Key.meta ]
  let is_modifier k = List.mem k key_modifiers

  let typing keys =
    let rec go acc i =
      if i >= String.length keys
      then acc
      else if Str.string_match Key.re_unicode keys i
      then let key = String.sub keys i 6 in
           if is_modifier key
           then `down key :: go (`up key :: acc) (i + 6)
           else `down key :: `up key :: acc @ go [] (i + 6)
      else let key = String.sub keys i 1 in
           `down key :: `up key :: acc @ go [] (i + 1)
    in
    go [] 0

  module Cookie = struct

    type same_site = [ `Lax | `Strict | `Undef ]

    type t =
      { name : string
      ; value : string
      ; path : string
      ; domain : string option
      ; secure : bool
      ; http_only : bool
      ; expiry : int
      ; same_site : same_site
      }

    let make ?(path = "/")
             ?domain
             ?(secure = false)
             ?(http_only = false)
             ?(expiry = 999)
             ?(same_site = `Undef)
             ~name
             ~value
             ()
    = { name
      ; value
      ; path
      ; domain
      ; secure
      ; http_only
      ; expiry
      ; same_site
      }

    let samesite_of_json = function
      | `String "Lax" -> `Lax
      | `String "Strict" -> `Strict
      | _ -> `Undef

    let t_of_json json =
      { name = J.string json.%("name")
      ; value = J.string json.%("value")
      ; path = J.string ~default:"/" json.%("path")
      ; domain = J.string_option json.%("domain")
      ; secure = J.bool ~default:false json.%("secure")
      ; http_only = J.bool ~default:false json.%("httpOnly")
      ; expiry = J.int json.%("expiry")
      ; same_site = samesite_of_json json.%("sameSite")
      }

    let json_of_samesite = function
      | `Lax -> `String "Lax"
      | `Strict -> `String "Strict"
      | `Undef -> `Null

    let nullable_string = function
      | None -> `Null
      | Some str -> `String str

    let json_of_t t =
      `Assoc [ "name", `String t.name
             ; "value", `String t.value
             ; "path", `String t.path
             ; "domain", nullable_string t.domain
             ; "secure", `Bool t.secure
             ; "httpOnly", `Bool t.http_only
             ; "expiry", `Int t.expiry
             ; "sameSite", json_of_samesite t.same_site
             ]

    let all =
      let+ json = get "/cookie" in
      List.map t_of_json (J.list json)

    let get name = t_of_json |<< get ("/cookie/" ^ name)

    let add cookie =
      let json = (`Assoc ["cookie", json_of_t cookie]) in
      J.unit |<< post "/cookie" json
  end

  module Alert = struct
    let dismiss = J.unit |<< post "/alert/dismiss" `Null
    let accept = J.unit |<< post "/alert/accept" `Null
    let get_text = J.string_option |<< get "/alert/text"
    let set_text txt =
      J.unit |<< post "/alert/text" (`Assoc ["text", `String txt])
  end

  module Timeouts = struct

    type t =
      { script : [`Never | `After of int]
      ; page_load : int
      ; implicit_wait : int
      }

    let get =
      let+ json = get "/timeouts" in
      let script = match json.%("script") with
        | `Null -> `Never
        | `Int ms -> `After ms
        | json -> Error.protocol_fail "expected null or int" json
      in
      { script
      ; page_load = J.int json.%("pageLoad")
      ; implicit_wait = J.int json.%("implicit")
      }

    let set ?script ?page_load ?implicit_wait () =
      let script = match script with
        | None -> []
        | Some `Never -> ["script", `Null]
        | Some (`After ms) -> ["script", `Int ms]
      in
      let page_load = match page_load with
        | None -> []
        | Some ms -> ["pageLoad", `Int ms]
      in
      let implicit_wait = match implicit_wait with
        | None -> []
        | Some ms -> ["implicit", `Int ms]
      in
      let json = `Assoc (script @ page_load @ implicit_wait) in
      J.unit |<< post "/timeouts" json
  end

  module Wait = struct

    let recoverable_errors =
      [ `element_not_interactable
      ; `invalid_element_state
      ; `move_target_out_of_bounds
      ; `no_such_alert
      ; `no_such_cookie
      ; `no_such_element
      ; `no_such_frame
      ; `no_such_window
      ]

    let wait dt = sleep dt

    let retry ?(max = 3000) ?(sleep = 50) ?(errors = recoverable_errors) cmd =
      let rec go fuel =
        if fuel < 0
        then cmd
        else Error.catch
                (fun () -> cmd)
                ~errors
                (fun _ -> let* () = wait sleep in go (fuel - sleep))
      in
      go max


    let condition_error = `unspecified "condition"
    let fail_condition =
      { Error.error = condition_error
      ; message = "Wait.until condition is unsatisfied"
      ; data = (`Null : json)
      ; stacktrace = ""
      }

    let until ?max ?sleep ?(errors = recoverable_errors) condition =
      let fail () = Error.fail (Webdriver fail_condition) in
      let cmd =
        let* ok = condition in
        if ok
        then return ()
        else fail ()
      in
      retry ?max ?sleep ~errors:(condition_error :: errors) cmd
  end

end

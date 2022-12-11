module type HTTP_CLIENT = sig

  type 'a t

  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  (** Interactions may raise exceptions: *)

  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  (** The following HTTP methods are required: *)

  val get : string -> string t
  (** [get url] returns the body of the response. *)

  val post : string -> string -> string t
  (** [post url body] returns the body of the response.
      The [Content-Type] header should be [application/json]. *)

  val delete : string -> string t
  (** [delete url] returns the body of the response. *)
end

module Error = struct
  type error =
    [ `element_click_intercepted
    | `element_not_interactable
    | `insecure_certificate
    | `invalid_argument
    | `invalid_cookie_domain
    | `invalid_element_state
    | `invalid_selector
    | `invalid_session_id
    | `javascript_error
    | `move_target_out_of_bounds
    | `no_such_alert
    | `no_such_cookie
    | `no_such_element
    | `no_such_frame
    | `no_such_window
    | `script_timeout
    | `session_not_created
    | `stale_element_reference
    | `timeout
    | `unable_to_set_cookie
    | `unable_to_capture_screen
    | `unexpected_alert_open
    | `unknown_command
    | `unknown_error
    | `unknown_method
    | `unsupported_operation
    | `unspecified of string (** browser specific failure *)
    | `ocaml_protocol_failure (** a bug in this library, please report! *)
    ]

  type t =
    { error : error (** the WebDriver error *)
    ; message : string (** a human readable explanation *)
    ; stacktrace : string (** browser stacktrace *)
    ; data : Yojson.Safe.t (** additional metadatas *)
    }
end

module type S = sig

  (** *)

  (**
  WebDriver is a {{: https://www.w3.org/TR/webdriver/} W3C specification} to
  remote control a web browser. This allow you to simulate and test user
  interactions on your website in real life conditions, with javascript
  enabled, on as many browsers and operating systems you can get your hands on.
  *)

  type json = Yojson.Safe.t

  type 'a io
  (** The client I/O monad, within which communication happens
      with the WebDriver server. *)

  (** {1 Commands} *)

  type session
  (** A connection to the WebDriver server.
      A session statefully holds the active windows, tabs, cookies, and other
      state required by the browser. *)

  type 'a cmd = session:session -> 'a io
  (** Every browser command takes place within a [session]. *)

  (** Since the [~session] parameter is generally constant,
      this module provides a reader monad to sequence multiple commands within
      the same session. You can either pass explicitly the [~session] argument
      or open this module.
  *)
  module Infix : sig

    val lift : 'a io -> 'a cmd
    (** [lift io] wraps an I/O action as a WebDriver command. *)

    val return : 'a -> 'a cmd

    val map : ('a -> 'b) -> 'a cmd -> 'b cmd

    val map2 : ('a -> 'b -> 'c) -> 'a cmd -> 'b cmd -> 'c cmd

    val bind : ('a -> 'b cmd) -> 'a cmd -> 'b cmd

    val ( >>| ) : 'a cmd -> ('a -> 'b) -> 'b cmd
    (** [cmd >>| fn] is [map fn cmd]. *)

    val ( |<< ) : ('a -> 'b) -> 'a cmd -> 'b cmd
    (** [fn |<< cmd] is [map fn cmd]. *)

    val ( <*> ) : ('a -> 'b) cmd -> 'a cmd -> 'b cmd
    (** [fn <*> arg] is [map2 (fun f x -> f x) fn arg]. *)

    val ( >>= ) : 'a cmd -> ('a -> 'b cmd) -> 'b cmd
    (** [cmd >>= fn] is [bind fn cmd]. *)

    val ( =<< ) : ('a -> 'b cmd) -> 'a cmd -> 'b cmd
    (** [fn =<< cmd] is [bind fn cmd]. *)

    val ( let+ ) : 'a cmd -> ('a -> 'b) -> 'b cmd
    (** [let+ x = cmd in e] is [map (fun x -> e) cmd] *)

    val ( let* ) : 'a cmd -> ('a -> 'b cmd) -> 'b cmd
    (** [let* x = cmd in e] is [bind (fun x -> e) cmd] *)

    val ( and* ) : 'a cmd -> 'b cmd -> ('a * 'b) cmd
    val ( and+ ) : 'a cmd -> 'b cmd -> ('a * 'b) cmd
  end

  (** All potentital errors raised by the WebDriver protocol. *)
  module Error : sig

    (** See the
        {{: https://www.w3.org/TR/webdriver/#errors} WebDriver specification}
        for an explanation of the source of an error: *)

    include module type of Error (** @inline *)

    val to_string : t -> string
    (** [to_string err] returns a debug-friendly string
        of the WebDriver error. *)

    val catch : (unit -> 'a cmd)
            -> ?errors:error list
            -> (t -> 'a cmd)
            -> 'a cmd
    (** [catch (fun () -> cmd) ?errors (fun err -> catch)] runs [cmd], catching
        any WebDriver exceptions from [?errors] (or all if unspecified),
        then runs [catch] if necessary with the error [err] as an argument.

        Some errors are less fatal than others: it's common to catch
        [`no_such_element], sleep and retry when the element in question is
        created by some (slow) javascript. See {! Timeouts} to add an implicit
        wait for this situation.
    *)

    val fail : exn -> 'a cmd
    (** [fail e] raises the exception [e]. *)
  end

  exception Webdriver of Error.t
  (** Every command that fails raises this exception, which contains some hint
      as to what went wrong. *)


  (** {1 Sessions}

      In order to create a [session], a connection to a WebDriver-compatible
      browser must be established:

    - The [host] is a url where the WebDriver server can be accessed,
      for example ["http://localhost:4444"].
    - The [capabilities] describe the requested browser settings.

  *)

  (** The requested capabilities when creating a new session. *)
  module Capabilities : sig

    (** See the
        {{: https://www.w3.org/TR/webdriver/#capabilities}
         WebDriver specification}
        and the documentation of the chosen browser for more informations. *)

    type t = json
    (** A json describing the required capabilities for the session. *)

    (** The capabilities are highly specific to the targetted browser. Here are
        some examples to get you started: *)

    val chrome : t
    (** The default [chrome] configuration:
    {[
      { "capabilities":
          { "firstMatch": [{ "browser_name": "chrome" } ] } }
    ]}
    *)

    val chrome_headless : t
    (** Same as [chrome],
        but runs in the background without a graphical window:
    {[
      { "capabilities":
          { "firstMatch":
              [{ "browser_name": "chrome",
                 "goog:chromeOptions": { "args": [ "--headless"
                                                 , "--disable-gpu"
                                                 , "--no-sandbox"
                                                 , "--disable-dev-shm-usage"
                                                 , "--window-size=1920,1080"
                                                 ] }
               }
              ]
          }
      }
    ]}
    *)

    val firefox : t
    (** The default [firefox] configuration:
    {[
      { "capabilities":
          { "firstMatch": [{ "browser_name": "firefox" } ] } }
    ]}
    *)

    val firefox_headless : t
    (** Same as [firefox],
        but runs in the background without a graphical window:
    {[
      { "capabilities":
          { "firstMatch":
              [{ "browser_name": "firefox",
                 "moz:firefoxOptions": { "args": ["-headless"] }
               }
              ]
          }
      }
    ]}
    *)
  end

  (** For creating and deleting sessions manually. *)
  module Session : sig
    (** A Session is a connection to the remote WebDriver browser (server). *)

    val make : host:string -> Capabilities.t -> (session * json) io
    (** [make ~host cap] creates a new session on [host] with the required
        [cap]abilities. It returns the created [session] and a [json]
        describing the available capabilities of the session.
     *)

    val delete : unit cmd
    (** [delete ~session] closes the [session] and remove all of its windows,
        cookies, etc. *)
  end

  val run : host:string -> Capabilities.t -> 'a cmd -> 'a io
  (** [run ~host capabilities cmd] is a helper function to create a new
      session on [host] with the required [capabilities], execute the
      [cmd] within that session, and finally ensure that the session is
      deleted on termination of [cmd] (from its natural death
      or an exception.)
   *)

  (** Configure the timeouts for page loads, script execution and the
      implicit wait when searching for elements on a page. *)
  module Timeouts : sig

    (**
    - The [script] timeout specifies when to interrupt a script that is being
      evaluated.

    - The [page_load] limits the time it takes to navigate
      and load an url.

    - The [implicit_wait] adds a delay to every {! find_first}
      and {! find_all}, to leave time for the searched element to be created.
    *)

    type t =
      { script : [`Never | `After of int]
        (** default is [`After 30_000] ms (= 3s) *)
      ; page_load : int
        (** default is [300_000] ms (= 300s) *)
      ; implicit_wait : int
        (** default is [0] ms *)
      }

    val get : t cmd
    (** [get] the current timeouts configuration. *)

    val set : ?script:[`Never | `After of int]
           -> ?page_load: int
           -> ?implicit_wait: int
           -> unit
           -> unit cmd
    (** [set ?script ?page_load ?implicit_wait ()] updates the configuration
        for the provided fields. *)
  end

  (** Even though the browser attempts to complete most operations before
      giving back control, some commands might trigger too soon and raise
      an error. The recommended strategy is to sleep and retry the
      operation repeatedly until it succeeds.
  *)
  module Wait : sig
    (** The default parameters are:
      - [?sleep = 50ms] is the amount of time to {! sleep}
        before retrying the command.
      - [?max = 3000ms] is the maximum total time spend waiting
        in between retries (in milliseconds).
      - [?errors] are the recoverable errors. By default:

      {[
      [ `element_not_interactable
      ; `invalid_element_state
      ; `move_target_out_of_bounds
      ; `no_such_alert
      ; `no_such_cookie
      ; `no_such_element
      ; `no_such_frame
      ; `no_such_window
      ]
      ]}

      The others WebDriver errors are more indicative of an issue in your
      automation script.
    *)

    val retry : ?max:int -> ?sleep:int -> ?errors:Error.error list
             -> 'a cmd -> 'a cmd
    (** [retry cmd] attempts to execute [cmd]. If it fails with a recoverable
        error, the execution sleeps for a bit to give the browser a chance to
        catch up, then the operation is retried until it succeeds or the [max]
        time is reached.

        {[ let* e = find_first `css "#id" in (* might raise `no_such_element *)
           (* vs *)
           let* e = Wait.retry @@ find_first `css "#id" in (* deterministic *)
        ]}
    *)

    val until : ?max:int -> ?sleep:int -> ?errors:Error.error list
             -> bool cmd -> unit cmd
    (** [until condition] behaves like [retry condition], but the predicate
        must also be satisfied at the end.

        Raises [`unspecified "condition"] if the [max] time is reached and
        the condition is still unsatisfied.

        {[ let* url = current_url in
           let* () = send_keys input ("hello" ^ Key.Enter) in
           let* () = Wait.until @@ map (( <> ) url ) current_url in
           (* blocks until the form is actually submitted. *)
        ]}
    *)
  end

  (** {1 Navigation} *)

  val goto : string -> unit cmd
  (** [goto url] ask the browser to visit the page at [url]. *)

  val current_url : string cmd
  (** The current url. *)

  val back : unit cmd
  (** Click the browser [back] button,
      to reload the previous url in history.  *)

  val forward : unit cmd
  (** Click the [forward] button,
      to move forward to the next url in history. *)

  val refresh : unit cmd
  (** Refresh the current url. *)

  (** Cookies management. *)
  module Cookie : sig

    type same_site = [`Lax | `Strict | `Undef]

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

    val make : ?path:string
            -> ?domain:string
            -> ?secure:bool
            -> ?http_only:bool
            -> ?expiry:int
            -> ?same_site: same_site
            -> name:string
            -> value:string
            -> unit
            -> t
    (** A helper function to create a new cookie with some optional parameters.
        The resulting cookie will not be set in the browser
        ands needs to be {! add}ed. *)

    val add : t -> unit cmd
    (** [add cookie] creates or updates the cookie in the browser. *)

    val all : t list cmd
    (** Returns the list of all cookies set in the browser. *)

    val get : string -> t cmd
    (** [get cookie_name] returns the configuration of the cookie with the
        specified name. Raises [`no_such_cookie] otherwise. *)
  end

  (** {1 HTML Elements} *)

  type elt (** An HTML element. *)

  type using =
    [ `css (** CSS selectors, like ["h1 span"] *)
    | `link_text (** The exact text in an [<a>...</a>] *)
    | `partial_link_text (** The partial text present in a link *)
    | `tag_name (** the HTML tag name, like ["h1"] or ["div"] *)
    | `xpath (** XPath query *)
    ]
  (** A strategy to find an element on a page. *)

  val find_first : ?from:elt -> using -> string -> elt cmd
  (** [find_first `using "query"] returns the first element
      that matches the [query] (interpreted with [using]).
      The element is searched inside the current frame of the current window,
      and if a [?from] parent element is provided, the search takes place
      inside it.

      [raise (Webdriver { error = `no_such_element ; _ })] otherwise.

      {[let* elt = find_first `css "h1 a" in ...
        let* elt = find_first `xpath "//h1//a" in ...]}

  *)

  val find_all : ?from:elt -> using -> string -> elt list cmd
  (** [find_all `using "query"] behaves like {! find_first}, but
      returns a list of all elements matching the [query].
  *)

  val active : elt cmd
  (** The currently focused element on the page. *)

  (** {1 Inspecting HTML elements} *)

  val text : elt -> string cmd
  (** The inner [text] of an element, ignoring any other markup. *)

  val tag_name : elt -> string cmd
  (** The HTML tag of an element, for example ["div"]. *)

  val attribute : elt -> string -> string cmd
  (** [attribute e attr] returns the value of the HTML attribute [attr] of
      the element [e]. *)

  val property : elt -> string -> string option cmd
  (** [property e prop] returns [Some] value of the DOM property [prop] of
      the element [e], or [None] if undefined. *)

  val is_selected : elt -> bool cmd
  (** The boolean status of a checkbox, a radio or an option in a select. *)

  val is_enabled : elt -> bool cmd
  (** The boolean status of an interactive element. *)

  val is_displayed : elt -> bool cmd
  (** Determines if the element is visible. *)

  val css : elt -> string -> string cmd
  (** [css e prop] returns the computed value of the css property [prop]
      for the element [e]. *)

  val aria_role : elt -> string cmd
  (** [aria_role e] returns the accessibility role of the element [e].
      See {{: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles}
      ARIA Roles on MDN} *)

  val aria_label : elt -> json cmd
  (** [aria_role e] returns the accessibility label of the element [e].
      See {{: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-label_attribute}
      ARIA Labels on MDN} *)

  type rect = { x : float ; y : float ; width : float ; height : float }
  val rect : elt -> rect cmd
  (** The position and size of an element on the page. *)

  (** {1 Interactions} *)

  val click : elt -> unit cmd
  (** Performs a mouse click on this element. *)

  val double_click : elt -> unit cmd
  (** Performs a mouse double click on this element. *)

  val submit : elt -> unit cmd
  (** Submit a form element and its descendents. *)

  val clear : elt -> unit cmd
  (** Clears the content of an input element. *)

  val send_keys : elt -> string -> unit cmd
  (** [send_keys e str] sends the string [str] to an input element, as if typed
      from a keyboard.  For special keys like [enter] or [backspace], use the
      predefined values in the module [Key]:
      {[send_keys my_input ("hello" ^ Key.enter)]}
  *)

  (** Special keys on a keyboard. *)
  module Key : sig
    type t = string
    val arrow_down : t
    val arrow_left : t
    val arrow_right : t
    val arrow_up : t
    val enter : t
    val return : t
    val tab : t
    val alt : t
    val meta : t
    val shift : t
    val control : t
    val escape : t
    val f1 : t
    val f2 : t
    val f3 : t
    val f4 : t
    val f5 : t
    val f6 : t
    val f7 : t
    val f8 : t
    val f9 : t
    val f10 : t
    val f11 : t
    val f12 : t
    val help : t
    val home : t
    val end_ : t
    val insert : t
    val backspace : t
    val delete : t
    val cancel : t
    val clear : t
    val numpad0 : t
    val numpad1 : t
    val numpad2 : t
    val numpad3 : t
    val numpad4 : t
    val numpad5 : t
    val numpad6 : t
    val numpad7 : t
    val numpad8 : t
    val numpad9 : t
    val numpad_add : t
    val numpad_comma : t
    val numpad_decimal : t
    val numpad_divide : t
    val numpad_enter : t
    val numpad_multiply : t
    val numpad_subtract : t
    val page_down : t
    val page_up : t
    val pause : t
    val unidentified : t
    val zenkaku_hankaku : t
  end

  type action (** A sequence of interactions from a user device. *)

  val perform : action list -> unit cmd
  (** [perform actions] executes the sequence of actions
      for each input source.
      The actions are synchronized vertically, such that:

      {[
        perform [ mouse    [ `down button0 ; `pause  ; `up button0 ]
                ; keyboard [ `down "a"     ; `up "a" ; `pause      ]
                ]
      ]}

      - The mouse starts a left click and the keyboard presses "a"
        simultaneously;
      - Then the keyboard releases the "a" key;
      - And finally the mouse releases the left click.

      The [`pause] action does nothing and is used for synchronization.

      The pressed keys and buttons stay pressed at the end of
      the interaction, unless explicitly released.
  *)

  val release : unit cmd
  (** [release] any pending keys or button from previous interactions. *)

  (** {2 Timing actions} *)

  type pause =
    [ `noop (** do nothing *)
    | `pause of int (** wait for duration (in ms) *)
    ]

  val none : ?name:string -> pause list -> action
  (** An inoperative device, that can be used to time the duration
      of each vertical frame. *)

  val sleep : int -> unit cmd
  (** [sleep duration] waits for [duration] in milliseconds before
      continuing. *)

  (** {2 Keyboard actions} *)

  type key =
    [ pause
    | `down of Key.t (** press down the key *)
    | `up of Key.t (** release a pressed key *)
    ]
  (** A typing interaction from a keyboard. *)

  val keyboard : ?name:string -> key list -> action

  (** [keyboard keys] is an action that simulates the typing of [keys] from a
      keyboard. The currently {! active} element will receive the key events.
  *)

  val typing : string -> key list
  (** [typing keys] is a helper function to produce an alternating sequence of
      [`down key] and [`up key] to simulate the typing of [keys]:

      {[ typing "ab" = [`down "a" ; `up "a" ; `down "b" ; `up "b"] ]}

      The modifier {! Key.alt}, {! Key.control}, {! Key.meta} and {! Key.shift}
      will be pressed differently to trigger the desired shortcut:

      {[ typing (Key.control ^ "a") (* CTRL-A *)
            = [`down Key.control ; `down "a" ; `up "a" ; `up Key.control]
      ]}
  *)

  (** {2 Pointer actions} *)

  type move
  (** A pointer movement to a new location, taking some [duration] of time
      (in milliseconds).  The default [duration] is a teleportation in [0]ms.
  *)

  val absolute : ?duration:int -> int * int -> move
  (** [absolute (x, y)] moves the pointer to the position [(x, y)]
      measured from the top left of the document. *)

  val relative : ?duration:int -> int * int -> move
  (** [relative (dx, dy)] moves the pointer by [(dx, dy)]
      from its current location. *)

  val center : ?duration:int -> ?offset:int * int -> elt -> move
  (** [center ~offset:(dx, dy) elt] moves the pointer to the center
      of the element [elt] offsetted by [offset].
      The default [offset] is [(0, 0)]. *)

  type button = int (** An integer representing the nth button on a mouse. *)

  val button_left : button
  (** The left mouse button (at position [0]). *)

  val button_middle : button
  (** The middle mouse button (at position [1]). *)

  val button_right : button
  (** The right mouse button (at position [2]). *)

  type pointer =
    [ pause
    | `cancel (** cancel the pointer current action *)
    | `down of button (** press down the button *)
    | `up of button (** release a pressed button *)
    | `move of move (** move the pointer *)
    ]
  (** An action from a mouse/touch/pen device *)

  val mouse : ?name:string -> pointer list -> action
  (** [mouse actions] describes the movement, click, etc of a mouse pointer. *)

  val touch : ?name:string -> pointer list -> action
  (** [touch actions] describes the interactions of a touch finger device.
      If multiple touch devices are used in the same {! perform}, they must
      have different names. *)

  val pen : ?name:string -> pointer list -> action
  (** [pen actions] describes the interactions of a pencil. *)

  (** {2 Scroll wheel actions} *)

  type scroll
    (** A scroll movement, taking some [duration] of time in milliseconds. *)

  val scroll_absolute : ?duration:int -> ?x:int -> ?y:int -> unit -> scroll
  (** [scroll_absolute ~x ~y ()] resets the scrollbar such that the
      position [x, y] falls into view, as measured from the top of the page.
      The default value of [x] and [y] is 0. *)

  val scroll_to : ?duration:int -> ?dx:int -> ?dy:int -> elt -> scroll
  (** [scroll_to ~dx ~dy elt] resets the scrollbar such that the center of the
      element [elt], offsetted by [(dx, dy)], is inside the view.
      The default offset of [dx] and [dy] is 0. *)

  type wheel =
    [  pause
    | `scroll of scroll (** scroll to a position *)
    ]
  (** An action from the scroll wheel. *)

  val wheel : ?name:string -> wheel list -> action
  (** [wheel scrolls] performs the scrolling actions. *)


  (** {1 Document} *)

  val title : string cmd
  (** The page [title] of the current document. *)

  val source : string cmd
  (** The HTML [source] code of the current document. *)

  val print : string cmd
  (** The current page, printed as a PDF. *)

  val screenshot : ?elt:elt -> unit -> string cmd
  (** Returns a PNG screenshot of the current page,
      or of the provided [?elt]. *)

  val switch_to_frame : [`top | `id of int | `elt of elt] -> unit cmd
  (** Focus the selected frame inside the current document.

    - The [`top] frame is the current document root.
    - The [`id n] frame is the [n]th frame in the page.
    - The [`elt e] frame is the frame associated with the HTML element [e].
  *)

  val switch_to_parent_frame : unit cmd
  (** Focus the parent of the currently selected frame. *)

  (** Windows and tabs management. *)
  module Window : sig
    type t
    (** A handle to a browser window (or a tab) *)

    type hint = [`tab | `window]

    val make : hint -> (t * [hint | `other of json]) cmd
    (** Create a new window or tab, using [hint] as a suggestion. Returns a
        handle to the created window/tab and its actual kind. *)

    val current : t cmd
    (** The [current] browser window/tab. *)

    val switch_to : t -> unit cmd
    (** [switch_to w] sets the [current] window to [w]. *)

    val close : t list cmd
    (** [close ws] closes a list of windows. *)

    val all : t list cmd
    (** List of all the opened browser windows/tabs. *)

    type rect = { x : int ; y : int ; width : int ; height : int }

    val get_rect : rect cmd
    (** The size and position of the [current] window. *)

    val set_rect : rect -> rect cmd
    (** [set_rect r] attempts to resize the [current] window
        to match the rectangle [r]. *)

    val maximize : rect cmd
    (** Maximizes the current window. *)

    val minimize : rect cmd
    (** Minimizes the current window. *)

    val fullscreen : rect cmd
    (** Fullscreen the current window. *)
  end

  (** Popup management: alert, confirm and prompt *)
  module Alert : sig
    val accept : unit cmd
    val dismiss : unit cmd
    val get_text : string option cmd
    val set_text : string -> unit cmd
  end

  (** {1 Javascript execution} *)

  val execute : string -> json cmd
  (** [excute "js"] runs the [js] on the current page,
      returning its result in [json]. *)

  val execute_async : string -> json cmd
  (** [excute_async "js"] runs the [js] asynchronously on the current page.

      This function terminates when the javascript callback [arguments[0]] is
      called, and returns its parameter as json. This can be used to block
      until some component has initialized:

      {[
          let* _ =
            execute_async
              {| var k = arguments[0]; something.onload(k); |}
          in
          (* blocks until onload triggers k *)
      ]}
   *)

end

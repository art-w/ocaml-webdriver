module Make (Webdriver : Webdriver.S) = struct
  let url_of_file filename = "file://" ^ Sys.getcwd () ^ "/" ^ filename

  let url_a = url_of_file "test/a.html"
  let url_b = url_of_file "test/b.html"

  let test name cmds = (name, cmds)

  open Webdriver.Infix

  let navigation = test "navigation"
    begin
      let open Webdriver in

      let* () = goto url_a in
      let* ta = title in
      assert (ta = "Page A") ;
      let* u = current_url in
      assert (u = url_a) ;

      let* () = goto url_b in
      let* tb = title in
      assert (tb = "Page B") ;
      let* ub = current_url in
      assert (ub = url_b) ;

      let* () = back in
      let* u = current_url in
      assert (u = url_a) ;

      let* () = forward in
      let* u = current_url in
      assert (u = url_b) ;

      let* () = refresh in
      let* u = current_url in
      assert (u = url_b) ;

      return ()
    end

  let multiple_windows = test "multiple windows"
    begin
      let open Webdriver in

      let* w0 = Window.current in
      let* ws = Window.all in
      assert (ws = [w0]) ;

      let* w1, _ = Window.make `window in
      let* w0' = Window.current in
      assert (w0 = w0') ;

      let* () = Window.switch_to w1 in
      let* w1' = Window.current in
      assert (w1 = w1') ;

      let sort ws = List.sort Stdlib.compare ws in
      let* ws = Window.all in
      assert (sort ws = sort [ w0 ; w1 ]) ;

      let* () = Window.switch_to w0 in
      let* w0' = Window.current in
      assert (w0 = w0') ;

      let* remaining_windows = Window.close in
      assert (remaining_windows = [w1]) ;

      return ()
    end

  let window_rect = test "window resizing"
    begin
      let open Webdriver in

      let* r1 = Window.get_rect in
      assert (r1.x >= 0) ;
      assert (r1.y >= 0) ;
      assert (r1.width > 0) ;
      assert (r1.height > 0) ;

      let* r2 = Window.maximize in
      assert (r2.x >= 0) ;
      assert (r2.y >= 0) ;
      assert (r2.width >= r1.width) ;
      assert (r2.height >= r1.height) ;

      let* r2' = Window.get_rect in
      assert (r2 = r2') ;

      let* r3 = Window.fullscreen in
      assert (r3.x >= 0) ;
      assert (r3.y >= 0) ;
      assert (r3.width >= r2.width) ;
      assert (r3.height >= r2.height) ;

      let* r3' = Window.get_rect in
      assert (r3 = r3') ;

      let* r4 = Window.minimize in
      assert (r4.x >= 0) ;
      assert (r4.y >= 0) ;
      assert (r4.width <= r3.width) ;
      assert (r4.height <= r3.height) ;

      let* r4' = Window.get_rect in
      assert (r4 = r4') ;

      let my_rect = { Window.x = 42 ; y = 420 ; width = 500 ; height = 600 } in
      let* my_rect' = Window.set_rect my_rect in
      assert (my_rect = my_rect') ;
      let* my_rect'' = Window.get_rect in
      assert (my_rect = my_rect'') ;

      return ()
    end

  let source = test "source"
    begin
      let open Webdriver in
      let* () = goto url_a in
      let* html = source in
      let is_html = "<html><head>" in
      let prefix = String.sub html 0 (String.length is_html) in
      assert (prefix = is_html) ;
      return ()
    end

  let screenshot = test "screenshot"
    begin
      let open Webdriver in
      let* () = goto url_a in

      let* png = screenshot () in
      assert ("PNG" = String.sub png 1 3) ;

      let* elt = find_first `css "h1" in
      let* png_elt = screenshot ~elt () in
      assert ("PNG" = String.sub png_elt 1 3) ;

      assert (String.length png > String.length png_elt) ;
      return ()
    end

  let exec_js = test "execute javascript"
    begin
      let open Webdriver in
      let* () = goto url_a in

      let* json = execute "return 42" in
      assert (json = `Int 42) ;

      let t0 = Unix.gettimeofday () in
      let* json =
        execute_async
          {| var k = arguments[0];
             setTimeout(function() { k(666) }, 1000);
          |}
      in
      let t1 = Unix.gettimeofday () in
      assert (json = `Int 666) ;
      assert (t1 -. t0 > 1.0) ;
      return ()
    end

  let find = test "find"
    begin
      let open Webdriver in
      let* () = goto url_a in

      let* first_h1 = find_first `tag_name "h1" in
      let* all_h1 = find_all `tag_name "h1" in
      assert (List.length all_h1 = 2) ;
      assert (List.hd all_h1 = first_h1) ;

      let* all_links = find_all `css "a" in
      let* fst_link  = find_first `partial_link_text "link to B" in
      let* snd_link  = find_first `partial_link_text "another link" in
      assert (all_links = [ fst_link ; snd_link ]) ;

      let* fst_link' = find_first `link_text "first link to B" in
      assert (fst_link = fst_link') ;

      let* fst_link'' = find_first `css "p a" in
      assert (fst_link = fst_link'') ;

      let* snd_link' = find_first `link_text "another link to B" in
      assert (snd_link = snd_link') ;

      let* snd_link'' = find_first `xpath "//a[contains(text(), 'another')]" in
      assert (snd_link = snd_link'') ;

      return ()
    end

  let of_option = function
    | None -> assert false
    | Some v -> v

  let inspect = test "inspect"
    begin
      let open Webdriver in
      let* () = goto url_a in

      let* input = find_first `css "form input[type='string']" in
      let* name = attribute input "name" in
      assert (name = "foo") ;

      let* init_value = attribute input "value" in
      assert (init_value = "default value") ;

      let* () = send_keys input "hello" in

      let* new_value = attribute input "value" in
      assert (new_value = "default value") ;

      let* real_value = property input "value" in
      let real_value = of_option real_value in
      assert (real_value = "default valuehello") ;

      let* color = css input "background-color" in
      assert (List.mem color ["rgb(0, 128, 0)"; "rgba(0, 128, 0, 1)"]) ;

      return ()
    end

  let form_interact = test "form interaction"
    begin
      let open Webdriver in
      let* () = goto url_a in

      let* input = find_first `css "input[name='foo']" in
      let* () = send_keys input (Key.backspace ^ "able") in

      let* btn = find_first `css "input[type='submit']" in
      let* () = click btn in

      let* url = current_url in
      assert (url = url_b ^ "?foo=default+valuable") ;

      let* () = back in
      let* input = find_first `css "input[name='foo']" in
      let* () = clear input in
      let* () = send_keys input "again" in
      let* btn = find_first `css "input[type='submit']" in
      let* () = click btn in
      let* url = current_url in
      assert (url = url_b ^ "?foo=again") ;

      return ()
    end

  let perform = test "perform"
    begin
      let open Webdriver in
      let* () = goto url_a in

      let* input = find_first `css "input[name='foo']" in
      let* default = of_option |<< property input "value" in
      assert (default = "default value") ;

      let* () = click input in
      let* focus = active in
      assert (focus = input) ;

      let expected = [ `down "a" ; `up "a" ; `down "b" ; `up "b" ] in
      assert (typing "ab" = expected) ;
      let* () = perform [ keyboard (typing "ab") ] in
      let* str = of_option |<< property input "value" in
      assert (str = "default valueab") ;

      let erase_all_expected =
        [ `down Key.control
        ; `down "a"
        ; `up "a"
        ; `up Key.control
        ; `down Key.backspace
        ; `up Key.backspace
        ] in
      let erase_all = typing (Key.control ^ "a" ^ Key.backspace) in
      assert (erase_all = erase_all_expected) ;

      let* () = perform [ keyboard erase_all ] in
      let* str = of_option |<< property input "value" in
      assert (str = "") ;

      let* () = send_keys input "test" in
      let* str = of_option |<< property input "value" in
      assert (str = "test") ;

      let* () = perform [ keyboard (typing Key.enter) ] in
      let* () =
        Wait.until
          (let+ url = current_url in
           url = url_b ^ "?foo=test")
      in

      let* () = back in
      let* url = current_url in
      assert (url = url_a) ;

      let* input = find_first `css "input[name='foo']" in
      let* str = of_option |<< property input "value" in
      assert (str = "test") ;

      let* btn = find_first `css "input[type='submit']" in
      let* rect = rect btn in
      let* _ = Window.maximize in
      let btn_top_left = (1 + int_of_float rect.x, 1 + int_of_float rect.y) in
      let move = absolute ~duration:50 btn_top_left in

      let* () = click input in
      let* active = active in
      assert (active = input) ;

      let do_click = [ `down button_left ; `pause 50 ; `up button_left ] in

      let* () =
        perform
          [ mouse    (`move move :: `noop  :: do_click)
          ; keyboard [`down "z"  ; `up "z"]
          ]
      in

      let* () =
        Wait.until
          (let+ url = current_url in
           url = url_b ^ "?foo=testz")
      in

      let* () = back in
      let* input = find_first `css "input[name='foo']" in
      let* () = click input in

      let* btn = find_first `css "input[type='submit']" in
      let reset = absolute ~duration:50 (0, 0) in
      let move = center ~duration:50 btn in

      let* () =
        perform
          [ mouse    (`move reset :: `move move :: do_click)
          ; keyboard [`down "y"   ; `up "y"]
          ]
      in

      let* url = current_url in
      assert (url = url_b ^ "?foo=testzy") ;

      return ()
    end

  let alert_js = test "alert from js"
    begin
      let open Webdriver in
      let* () = goto url_b in

      let* btn = find_first `css "#alert" in
      let* txt = text btn in
      assert (txt = "Alert on double click") ;

      let* () = double_click btn in

      let* msg = Alert.get_text in
      assert (msg = Some "Hi from javascript") ;

      let* () = Alert.dismiss in

      let* txt = text btn in
      assert (txt = "Clicked!") ;

      return ()
    end



  let all =
    [ window_rect
    ; navigation
    ; multiple_windows
    ; source
    ; screenshot
    ; exec_js
    ; find
    ; inspect
    ; form_interact
    ; perform
    ; alert_js
    ]
end

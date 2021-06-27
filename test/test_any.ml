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

      let my_rect = { x = 42 ; y = 420 ; width = 500 ; height = 600 } in
      let* my_rect' = Window.set_rect my_rect in
      assert (my_rect = my_rect') ;
      let* my_rect'' = Window.get_rect in
      assert (my_rect = my_rect'') ;

      return ()
    end

  let all =
    [ window_rect
    ; navigation
    ; multiple_windows
    ]
end

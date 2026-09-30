(** The raw WebDriver library, not yet specialized to an I/O monad or an HTTP client.
    See {! Webdriver_cohttp_lwt_unix}, {! Webdriver_cohttp_async} or
    {! Webdriver_cohttp_eio}. *)

module type HTTP_CLIENT = sig include Webdriver_sig.HTTP_CLIENT (** @inline *) end

module type S = sig include Webdriver_sig.S (** @inline *) end

module Make (IO : HTTP_CLIENT) : S with type 'a io = 'a IO.t

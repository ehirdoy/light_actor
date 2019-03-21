open Lwt.Infix

include Actor_param_types.Make(Test.Impl)

module Main (S: Mirage_stack_lwt.V4) (KV: Mirage_kv_lwt.RO) = struct

  module N = Actor_net_mirage.Make (S)
  module M = Actor_param.Make (N) (Actor_sys_mirage) (Test.Impl)

  let test kv =
    let our_secret = "foo\n" in
    KV.get kv (Mirage_kv.Key.v "secret") >|= function
    | Error e ->
      Logs.warn (fun f -> f "Could not compare the secret against a known constant: %a"
        KV.pp_error e)
    | Ok stored_secret ->
      match String.compare our_secret stored_secret with
      | 0 ->
        Logs.info (fun f -> f "Contents of extremely secret vital storage confirmed!")
      | _ ->
        Logs.warn (fun f -> f "The secret provided does not match!")


  let start (s : S.t) kv =
    N.stored_stack_handler := Some s;

    let server_uuid = "server" in
    let server_ip = Key_gen.server_ip () in
    let server_port = Key_gen.server_port () in
    let server_addr = "tcp://" ^ server_ip ^ ":" ^ server_port in

    let my_uuid = Key_gen.uuid () in
    let my_ip = Key_gen.ip () in
    let my_port = Key_gen.port () in
    let my_addr = "tcp://" ^ my_ip ^ ":" ^ my_port in

    (* define the participants *)
    let book = Actor_book.make () in
    Actor_book.add book "w0" "" true (-1);
    Actor_book.add book "w1" "" true (-1);

    let my_addr =
      if my_uuid = server_uuid then
        server_addr
      else begin
        Actor_book.set_addr book my_uuid my_addr;
        my_addr
      end in

    Logs.info (fun f -> f "uuid=%s addr=%s server=%s" my_uuid my_addr server_addr);

    let context = {
      my_uuid;
      my_addr;
      server_uuid;
      server_addr;
      book;
    }
    in

    test kv >>= fun () ->
    M.init context

end

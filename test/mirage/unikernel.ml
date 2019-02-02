module Net_mirage_udp = struct
  type socket = int

  let init () =
    Logs.info (fun f -> f "Net_impl.init ()");
    Random.self_init();
    Lwt.return_unit

  let exit () =
    Logs.info (fun f -> f "Net_impl.exit ()");
    Lwt.return_unit

  let listen _addr _callback =
    Logs.info (fun f -> f "Net_impl.listen () %s" _addr);
    Lwt.return_unit

  let send _addr _data =
    Logs.info (fun f -> f "Net_impl.send () %s" _addr);
    Lwt.return_unit

  let recv _sock =
    Logs.info (fun f -> f "Net_impl.recv ()");
    Lwt.return "" (* not used *)

  let close _sock =
    Logs.info (fun f -> f "Net_impl.close ()");
    Lwt.return_unit (* not used *)

end

module Impl = struct

  type model = (string, int) Hashtbl.t

  type key = string

  type value = int

  let start_t = ref 0

  let model : model =
    let htbl = Hashtbl.create 128 in
    Hashtbl.add htbl "a" 0;
    Hashtbl.add htbl "b" 0;
    Hashtbl.add htbl "c" 0;
    htbl

  let get keys =
    Array.map (Hashtbl.find model) keys

  let set kv_pairs =
    Array.iter (fun (key, value) ->
      Hashtbl.replace model key value
    ) kv_pairs

  let schd nodes =
    Array.map (fun node ->
      let i = Random.int 3 in
      let key = [|"a"; "b"; "c"|].(i) in
      let value = (get [|key|]).(0) in
      let tasks = [|(key, value)|] in
      (node, tasks)
    ) nodes

  let push kv_pairs =
    for i = 1_000_000 * (Random.int 3) to 0 do i |> ignore done;
    Array.map (fun (key, value) ->
      let new_value = value + Random.int 10 in
      Logs.info (fun f -> f "%s: %i => %i" key value new_value);
      (key, new_value)
    ) kv_pairs

  let pull updates = updates

  let stop () =
    Logs.info (fun f -> f "start_t = %i" !start_t);
    start_t := !start_t + 1;
    !start_t >= 50

end


include Actor_param_types.Make(Impl)

module M = Actor_param.Make (Net_mirage_udp) (Actor_sys_mirage) (Impl)

module Main (S: Mirage_stack_lwt.V4) = struct

  let start s =
    let server_uuid = "server" in
    let server_ip = Key_gen.server_ip () in
    let server_port = Key_gen.server_port () in
    let server_addr = "tcp://" ^ server_ip ^ ":" ^ server_port in

    let my_uuid = Key_gen.my_uuid () in
    let my_ip = Key_gen.my_ip () in
    let my_port = Key_gen.my_port () in
    let my_addr = "tcp://" ^ my_ip ^ ":" ^ my_port in
    let my_ip' = Ipaddr.V4.to_string (List.hd (S.IPV4.get_ip (S.ipv4 s))) in
    if my_ip <> my_ip' then Logs.info (fun f -> f "different IP address? %s vs %s" my_ip my_ip');

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

    Logs.info (fun f -> f "my_uuid=%s server=%s my=%s" my_uuid server_addr my_addr);

    (* define parameter server context *)
    let context = {
      my_uuid;
      my_addr;
      server_uuid;
      server_addr;
      book;
    }
    in

    M.init context

end

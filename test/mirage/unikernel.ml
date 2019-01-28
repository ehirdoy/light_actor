open Lwt.Infix

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
    Unix.sleep (Random.int 3);
    Array.map (fun (key, value) ->
      let new_value = value + Random.int 10 in
      Actor_log.info "%s: %i => %i" key value new_value;
      (key, new_value)
    ) kv_pairs

  let pull updates = updates

  let stop () =
    Actor_log.info "start_t = %i" !start_t;
    start_t := !start_t + 1;
    !start_t >= 50

end


include Actor_param_types.Make(Impl)

module M = Actor_param.Make (Actor_net_mirage) (Actor_sys_mirage) (Impl)

module Main (S: Mirage_stack_lwt.V4) = struct

  let report_and_close flow pp e message =
    let ip, port = S.TCPV4.dst flow in
    Logs.warn (fun m -> m "closing connection from %a:%d due to error %a while %s"
                  Ipaddr.V4.pp ip port pp e message);
    S.TCPV4.close flow

  let rec echo flow =
    S.TCPV4.read flow >>= function
    | Error e -> report_and_close flow S.TCPV4.pp_error e "reading in Echo"
    | Ok `Eof -> report_and_close flow Fmt.string "end of file" "reading in Echo"
    | Ok (`Data buf) ->
      Logs.debug (fun f -> f "%s" (Cstruct.to_string buf));
      S.TCPV4.write flow buf >>= function
      | Ok () -> echo flow
      | Error e -> report_and_close flow S.TCPV4.pp_write_error e "writing in Echo"

  let start s =
    let uuid = Key_gen.uuid () in
    Logs.info (fun f -> f "uuid=\"%s\"\n" uuid);
    S.listen_tcpv4 s ~port:5555 echo;
    S.listen s

end

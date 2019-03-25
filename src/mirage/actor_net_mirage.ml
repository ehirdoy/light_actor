(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Lwt.Infix

module Make (S : Mirage_stack_lwt.V4) = struct

  let prefix = "tcp://"

  let port_of_str addr =
    String.split_on_char ':' addr
    |> List.rev |> List.hd |> int_of_string

  let str_of_flow flow =
    let ip, port = S.TCPV4.dst flow in
    (Ipaddr.V4.to_string ip) ^ (string_of_int port)

  let to_ip s =
    let s2 =
      match String.split_on_char '/' s with
      | _ :: _ :: [x] -> x
      | _ -> failwith "Err" in
    String.split_on_char '/' s2 |> List.rev |> List.hd

  let ipport_of_str addr =
    let ip, port =
      match String.split_on_char ':' addr with
      | _ :: ip :: port :: [] -> ip, port
      | _ -> failwith "wrong format" in
    let dst_port = int_of_string port in
    let dst = Ipaddr.V4.of_string_exn (to_ip ip) in
    dst, dst_port


  module Conn = struct

    type t = {
      flow: S.TCPV4.flow;
      len:  int;
      bufs: bytes list;
    }

    let pool : (string, t) Hashtbl.t = Hashtbl.create 128

    let flow addr =
      let v = Hashtbl.find pool addr in
      v.flow

    let add addr flow bytes =
      try
        let v = Hashtbl.find pool addr in
        (* assert (v.flow = flow);
         * assert (v.len >= Bytes.length bytes); *)
        Hashtbl.replace pool addr
          {flow=v.flow; len=v.len; bufs=bytes::v.bufs}
      with Not_found ->
        let len = Marshal.total_size bytes 0 in
        Actor_log.debug "total_size=%d" len;
        Hashtbl.add pool addr
          {flow=flow; len=len; bufs=[bytes]}

    let is_full addr =
      let v = Hashtbl.find pool addr in
      let sum = List.fold_left
          (fun a el -> a + Bytes.length el) 0 v.bufs in
      Actor_log.debug "packet %d / %d" sum v.len;
      v.len = sum

    let bufs addr =
      let v = Hashtbl.find pool addr in
      let bytes = Bytes.concat Bytes.empty (List.rev v.bufs) in
      Hashtbl.remove pool addr;
      bytes

    let exit () =
      Hashtbl.iter (fun _ v ->
          Lwt.async (fun () -> S.TCPV4.close v.flow)
        ) pool
  end


  type socket = Conn.t

  let stored_stack_handler : S.t option ref = ref None
  let get_stack () = match !stored_stack_handler with
    | None -> failwith "Uninitialized s"
    | Some s -> s


  let init () =
    Lwt.return_unit

  let exit () =
    Conn.exit ();
    Lwt.return_unit

  let listen addr callback =
    let rec handler flow =
      let daddr = str_of_flow flow in
      S.TCPV4.read flow >>= function
      | Error _e -> failwith "S.TCPV4.read flow"
      | Ok `Eof -> Actor_log.debug "EOF from %s" daddr; Lwt.return_unit
      | Ok (`Data buf) ->
        Actor_log.debug "READ from %s %d Bytes" daddr (Cstruct.len buf);
        Conn.add daddr flow (Cstruct.to_bytes buf);
        if Conn.is_full daddr then
          callback (Bytes.to_string (Conn.bufs daddr)) >>=
          fun () -> handler flow
        else
          handler flow
    in
    let s = get_stack () in
    S.listen_tcpv4 s ~port:(port_of_str addr) handler;
    S.listen s

  let send addr data =
    let%lwt flow =
      try
        Lwt.return (Conn.flow addr)
      with Not_found ->
        S.TCPV4.create_connection (S.tcpv4 (get_stack ())) (ipport_of_str addr)
        >>= function
        | Error _err -> failwith "S.TCPV4.create_connection"
        | Ok flow ->
          Conn.add addr flow Bytes.empty;
          Lwt.return flow
    in
    let%lwt _ = S.TCPV4.write flow (Cstruct.of_string data) in
    Lwt.return_unit

  let recv _sock =
    Lwt.return "" (* not used *)

  let close _sock =
    Lwt.return_unit (* not used *)

end

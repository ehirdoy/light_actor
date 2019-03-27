(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Lwt.Infix

module Make (S : Mirage_stack_lwt.V4) = struct

  type socket = {
    flow: S.TCPV4.flow;
    len:  int;
    bufs: bytes list;
  }

  let prefix = "tcp://"

  let conn_pool : (string, socket) Hashtbl.t = Hashtbl.create 128

  let stored_stack_handler : S.t option ref = ref None

  let to_ip s =
    let s2 =
      match String.split_on_char '/' s with
      | _ :: _ :: [x] -> x
      | _ -> failwith "Err" in
    String.split_on_char '/' s2 |> List.rev |> List.hd

  let init () =
    Random.self_init();
    Lwt.return_unit

  let exit () =
    Hashtbl.iter (fun _ v ->
        Lwt.async (fun () -> S.TCPV4.close v.flow)
      ) conn_pool;
    Lwt.return_unit

  let pbuf_add addr flow bytes =
    if Hashtbl.mem conn_pool addr then
      let v = Hashtbl.find conn_pool addr in
      (* assert (v.flow = flow);
       * assert (v.len >= Bytes.length bytes); *)
      Hashtbl.replace conn_pool addr
        {flow=v.flow; len=v.len; bufs=bytes::v.bufs}
    else
      let len = Marshal.total_size bytes 0 in
      Actor_log.debug "total_size=%d" len;
      Hashtbl.add conn_pool addr
        {flow=flow; len=len; bufs=[bytes]}

  let pbuf_is_full addr =
    let v = Hashtbl.find conn_pool addr in
    let sum = List.fold_left (fun a el -> a + Bytes.length el) 0 v.bufs in
    Actor_log.debug "packet %d / %d" sum v.len;
    v.len = sum

  let pbuf_bufs addr =
    let v = Hashtbl.find conn_pool addr in
    let bytes = Bytes.concat Bytes.empty (List.rev v.bufs) in
    Hashtbl.remove conn_pool addr;
    bytes

  let listen addr callback =
    let port = String.split_on_char ':' addr
               |> List.rev |> List.hd |> int_of_string in
    let s = match !stored_stack_handler with
      | None -> failwith "Uninitialized s"
      | Some s -> s in

    let rec fn flow =
      let ip, port = S.TCPV4.dst flow in
      let daddr = (Ipaddr.V4.to_string ip) ^ (string_of_int port) in
      S.TCPV4.read flow >>= function
      | Error e -> assert false
      | Ok `Eof -> Actor_log.debug "EOF from %s" daddr; Lwt.return_unit
      | Ok (`Data buf) ->
        Actor_log.debug "READ from %s %d Bytes" daddr (Cstruct.len buf);
        pbuf_add daddr flow (Cstruct.to_bytes buf);
        if pbuf_is_full daddr then
          callback (Bytes.to_string (pbuf_bufs daddr)) >>= fun () -> fn flow
        else
          fn flow
    in

    S.listen_tcpv4 s ~port fn;
    S.listen s

  let send addr data =
    let%lwt flow =
      if Hashtbl.mem conn_pool addr then (
        Actor_log.debug "Found connection with %s" addr;
        let v = Hashtbl.find conn_pool addr in
        assert (v.len = 0);
        assert ((List.length v.bufs) = 0);
        Lwt.return v.flow
      ) else (
        let ip, port =
          match String.split_on_char ':' addr with
          | _ :: ip :: port :: [] -> ip, port
          | _ -> failwith "wrong format" in
        let dst_port = int_of_string port in
        let dst = Ipaddr.V4.of_string_exn (to_ip ip) in
        let s = match !stored_stack_handler with
          | None -> failwith "Uninitialized s"
          | Some s -> s
        in
        let rec loop () =
          S.TCPV4.create_connection (S.tcpv4 s) (dst, dst_port) >>= function
          | Error _err ->
            Actor_log.warn "failed to connect with %s" addr;
            loop ()
          | Ok flow ->
            Actor_log.debug "create_connection with %s" addr;
            Hashtbl.add conn_pool addr {flow=flow;len=0;bufs=[]};
            Lwt.return flow
        in
        loop ()
      )
    in
    Actor_log.debug "Sending to %s with %d Bytes" addr (String.length data);
    let%lwt _ = S.TCPV4.write flow (Cstruct.of_string data) in
    Lwt.return_unit

  let recv _sock =
    Lwt.return "" (* not used *)

  let close _sock =
    Lwt.return_unit (* not used *)

end

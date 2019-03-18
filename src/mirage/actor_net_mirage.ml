(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Lwt.Infix

module Make (S : Mirage_stack_lwt.V4) = struct

  type socket

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
    Lwt.return_unit

  type data_buff_t = { total: int; buffs: bytes list }
  type sockaddr_t = { ip: Ipaddr.V4.t; port: int }
  let data_pool : ((sockaddr_t, data_buff_t) Hashtbl.t) = Hashtbl.create 128

  let data_pool_is_filled saddr =
    let data_buff = Hashtbl.find data_pool saddr in
    let sum = List.fold_left (fun a el -> a + Bytes.length el) 0 data_buff.buffs in
    Logs.info (fun f -> f "sum=%d total=%d" sum data_buff.total);
    sum >= data_buff.total

  let data_pool_buffs saddr =
    let data_buff = Hashtbl.find data_pool saddr in
    let bytes = Bytes.concat Bytes.empty (List.rev data_buff.buffs) in
    Hashtbl.remove data_pool saddr;
    bytes

  let data_pool_process saddr cbuf callback =
    let bytes = Cstruct.to_bytes cbuf in
    let data_buff =
      if Hashtbl.mem data_pool saddr then
        let orig = Hashtbl.find data_pool saddr in
        { total=orig.total; buffs= bytes :: orig.buffs; }
      else
        { total=Marshal.total_size bytes 0; buffs=[bytes]; }
    in
    Hashtbl.replace data_pool saddr data_buff;
    if data_pool_is_filled saddr then
      callback (Bytes.to_string (data_pool_buffs saddr))
    else
      Lwt.return_unit

  let listen addr callback =
    let port = String.split_on_char ':' addr
               |> List.rev |> List.hd |> int_of_string in
    let cb _udp port ~src ~dst ~src_port buf =
      Logs.info (fun f -> f "%a:%d -> %a:%d"
                    Ipaddr.V4.pp src src_port Ipaddr.V4.pp dst port);
      data_pool_process {ip=src; port=port;} buf callback
    in
    let s = match !stored_stack_handler with
      | None -> failwith "Uninitialized s"
      | Some s -> s in
    S.listen_udpv4 s ~port (cb (S.udpv4 s) port);
    S.listen s

  let send addr data =
    let ip, port =
      match String.split_on_char ':' addr with
      | _ :: ip :: port :: [] -> ip, port
      | _ -> failwith "wrong format" in
    let dst_port = int_of_string port in
    let dst = Ipaddr.V4.of_string_exn (to_ip ip) in
    let s = match !stored_stack_handler with
      | None -> failwith "Uninitialized s"
      | Some s -> s in
    S.UDPV4.write ~dst ~dst_port (S.udpv4 s) (Cstruct.of_string data)
    >>= fun _ -> Lwt.return_unit

  let recv _sock =
    Lwt.return "" (* not used *)

  let close _sock =
    Lwt.return_unit (* not used *)

end

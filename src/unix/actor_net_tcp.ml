(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2019 Hiroshi Doyu <hiroshi.doyu@ericsson.com>
 *)

open Unix

type socket = Lwt_unix.file_descr

let conn_pool : (string, socket) Hashtbl.t = Hashtbl.create 128

(* str : "tcp://127.0.0.1:6123" *)
let saddr_of str =
  match String.split_on_char ':' str with
  | "tcp" :: a :: p :: [] ->
    let addr = String.map (fun c -> if c = '/' then ' ' else c) a
               |> String.trim
               |> Unix.inet_addr_of_string in
    let port = int_of_string p in
    Lwt_unix.ADDR_INET (addr, port)
  | _ -> failwith "Invalid param"


let init () =
  Random.self_init();
  Lwt.return_unit

let exit () =
  Hashtbl.iter (fun _ v ->
      Lwt.async (fun () -> Lwt_unix.close v)
    ) conn_pool;
  Lwt.return_unit


let listen addr callback =
  let rec fn buf (ic, oc) =
    match%lwt Lwt_io.read_into ic buf 0 (Bytes.length buf) with
    | _ ->
      let%lwt () = callback (Bytes.to_string buf) in
      fn buf (ic, oc)
  in
  Lwt_io.establish_server_with_client_address
    (saddr_of addr)
    (fun _ (ic, oc) ->
       fn (Bytes.create (16 * 1024)) (ic, oc)) |> ignore;
  Lwt.return_unit


let send addr data =
  let%lwt sock =
    if Hashtbl.mem conn_pool addr then (
      Lwt.return (Hashtbl.find conn_pool addr)
    )
    else (
      let s = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      let%lwt _ = Lwt_unix.connect s (saddr_of addr) in
      Hashtbl.add conn_pool addr s;
      Lwt.return s
    )
  in
  let msg = Bytes.of_string data in
  let len = String.length data in
  let%lwt _ = Lwt_unix.send sock msg 0 len [] in
  Lwt.return_unit


let recv _sock =
  Lwt.return "" (* not used *)


let close _sock =
  Lwt.return_unit (* not used *)

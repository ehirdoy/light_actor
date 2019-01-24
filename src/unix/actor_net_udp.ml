(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2019 Hiroshi Doyu <hiroshi.doyu@ericsson.com>
 *)

open Lwt.Infix

type socket = Lwt_unix.file_descr

let conn_pool : (string, socket) Hashtbl.t = Hashtbl.create 128

(* str : "tcp://127.0.0.1:6123" *)
let saddr_of str =
  match String.split_on_char ':' str with
  | "udp" :: a :: p :: [] ->
    let addr = String.map (fun c -> if c = '/' then ' ' else c) a
               |> String.trim
               |> Unix.inet_addr_of_string in
    let port = int_of_string p in
    Lwt_unix.ADDR_INET (addr, port)
  | _ -> failwith "Invalid param"


let init () =
  Random.self_init ();
  Lwt.return_unit


let exit () =
  Hashtbl.iter (fun _ v ->
      Lwt.async (fun () -> Lwt_unix.close v)
    ) conn_pool;
  Lwt.return_unit


let listen addr callback =
  Lwt.async (fun () ->
      let sock = Lwt_unix.socket PF_INET SOCK_DGRAM 0 in
      let%lwt _ = Lwt_unix.bind sock (saddr_of addr) in
      let buf = Cstruct.create 4096 in
      let rec loop () =
        Lwt.catch (fun () ->
            Lwt_cstruct.recvfrom sock buf [] >>= fun (len, sa) ->
            let buf = Cstruct.sub buf 0 len in
            (match sa with
             | Lwt_unix.ADDR_INET (_addr, _src_port) ->
               callback (Cstruct.to_string buf)
             | _ -> Lwt.return_unit))
          (fun exn ->
             Printf.printf
               "exception %s in recvfrom" (Printexc.to_string exn);
             Lwt.return_unit) >>= fun () ->
        loop ()
      in
      loop ());
  Lwt.return_unit


let send addr data =
  let%lwt sock =
    if Hashtbl.mem conn_pool addr then (
      Lwt.return (Hashtbl.find conn_pool addr)
    )
    else (
      let s = Lwt_unix.socket PF_INET SOCK_DGRAM 0 in
      Hashtbl.add conn_pool addr s;
      Lwt.return s
    )
  in
  let msg = Bytes.of_string data in
  let len = String.length data in
  let%lwt _ = Lwt_unix.sendto sock msg 0 len [] (saddr_of addr) in
  Lwt.return_unit


let recv _sock =
  Lwt.return "" (* not used *)

let close _sock =
  Lwt.return_unit (* not used *)

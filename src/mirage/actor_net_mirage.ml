(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type socket = int

let init () =
  Random.self_init();
  Lwt.return_unit

let exit () =
  Lwt.return_unit


let listen _addr _callback =
  Lwt.return_unit


let send _addr _data =
  Lwt.return_unit


let recv _sock =
  Lwt.return "" (* not used *)


let close _sock =
  Lwt.return_unit (* not used *)

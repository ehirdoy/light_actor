(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module type Sig = sig

  type socket

  type +'a io

  val init : unit -> unit io

  val exit : unit -> unit io

  val listen : string -> (string -> unit io) -> unit io

  val send : string -> string -> unit io

  val recv : socket -> string io

  val close : socket -> unit io

end

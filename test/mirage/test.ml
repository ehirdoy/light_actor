(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

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

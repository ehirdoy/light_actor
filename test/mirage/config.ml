open Mirage

let uuid =
  let doc = Key.Arg.info ~doc:"uuid of service" ["uuid"] in
  Key.(create "uuid" Arg.(opt string "server" doc))


let packages = [package "actor"; package "actor_mirage"; package "duration"]
let main =
  foreign ~packages ~keys:[Key.abstract uuid]
    "Unikernel.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "actor" [main $ stack]

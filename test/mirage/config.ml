open Mirage

let my_uuid =
  let doc = Key.Arg.info ~doc:"uuid of service" ["my_uuid"] in
  Key.(create "my_uuid" Arg.(opt string "server" doc))

let server_ip =
  let doc = Key.Arg.info ~doc:"server IP address" ["server_ip"] in
  Key.(create "server_ip" Arg.(opt string "127.0.0.1" doc))

let server_port =
  let doc = Key.Arg.info ~doc:"server port." ["server_port"] in
  Key.(create "server_port" Arg.(opt string "5555" doc))

let my_ip =
  let doc = Key.Arg.info ~doc:"my IP address" ["my_ip"] in
  Key.(create "my_ip" Arg.(opt string "127.0.0.1" doc))

let my_port =
  let doc = Key.Arg.info ~doc:"my port." ["my_port"] in
  Key.(create "my_port" Arg.(opt string "6000" doc))

let main =
  let packages = [
    package "actor";
    package "actor_mirage";
    package "duration";
    package "lwt_ppx";
  ] in
  let keys = List.map Key.abstract [
      server_ip;
      server_port;
      my_uuid;
      my_ip;
      my_port;
    ] in
  foreign ~packages ~keys "Unikernel.Main" (stackv4 @-> job)

let () =
  let stack = generic_stackv4 default_network in
  register "lwae" [main $ stack]

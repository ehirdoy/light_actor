open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct

  let report_and_close flow pp e message =
    let ip, port = S.TCPV4.dst flow in
    Logs.warn (fun m -> m "closing connection from %a:%d due to error %a while %s"
                  Ipaddr.V4.pp ip port pp e message);
    S.TCPV4.close flow

  let rec echo flow =
    S.TCPV4.read flow >>= function
    | Error e -> report_and_close flow S.TCPV4.pp_error e "reading in Echo"
    | Ok `Eof -> report_and_close flow Fmt.string "end of file" "reading in Echo"
    | Ok (`Data buf) ->
      S.TCPV4.write flow buf >>= function
      | Ok () -> echo flow
      | Error e -> report_and_close flow S.TCPV4.pp_write_error e "writing in Echo"

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port echo;
    S.listen s

end

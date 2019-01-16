open Mirage

let main =
  foreign "Unikernel.Hello" job

let () =
  register "hello" [main]

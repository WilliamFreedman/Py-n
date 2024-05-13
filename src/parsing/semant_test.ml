open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program_rule Scanner.tokenize lexbuf in
  let sblock_list = Semant.check program.body in
  let sprog = { body = sblock_list } in
  print_endline (string_of_sprogram sprog)

open Macros
open Indents
let preprocessing = 
  if not (Sys.file_exists "temp_files") then
    Sys.mkdir "temp_files" 0o755;;

let file_name = Sys.argv.(1);;
Printf.printf "%s\n" (Sys.getcwd ());;
MacroProcessor.process_file ("./input_file/" ^ file_name);;

IndentFixup.indent_fixup file_name;;

let file_name = (Sys.argv.(1));;

Sys.chdir "preprocessing";;

Printf.printf "Beginning Preprocessing\n";

Sys.command "make";;


let pre_result = Sys.command ("./main " ^ file_name);;

if pre_result != 0 then begin
  Printf.printf "Preprocessing failure\n";
  exit 1;
  end
else
Printf.printf "Preprocessing successful\n";

Printf.printf "Beginning Parsing\n";

Sys.chdir "../parsing";



let make_result = Sys.command "ocamlbuild test.native" in

if make_result > 0 then Printf.printf "Parser build failed\n" else

let preprocessed_file_name = ("../preprocessing/temp_files/" ^ Filename.basename (Filename.chop_extension file_name) ^ "_i" ^ Filename.extension file_name) in
Printf.printf "%s\n %!" preprocessed_file_name;

let parse_result = Sys.command ("ocamlbuild test.native && ./test.native < " ^ preprocessed_file_name) in
if (parse_result > 0) then begin Printf.printf "Parse failure\n"; exit 1 end


(*

let parse_result = Sys.command ("ocamlbuild test.native && ./test.native < " ^ preprocessed_file_name) in
if parse_result != 0 then
  Printf.printf "Parsing failure\n"
else
  Printf.printf "Success!\n"
*)
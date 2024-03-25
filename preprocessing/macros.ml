module StringMap = Map.Make(String)

let parse_define_line line =
  if String.length line >= 8 && String.sub line 0 7 = "@DEFINE" then
    let parts = String.split_on_char ' ' line in
    match parts with
    | _ :: identifier :: value -> Some (identifier, String.concat " " value)
    | _ -> None
  else
    None

let apply_macros string_map line =
  let apply_macro_to_word word =
    if StringMap.mem word string_map then
      StringMap.find word string_map
    else
      word
  in
  String.concat " " (List.map apply_macro_to_word (String.split_on_char ' ' line))

let populate_string_map_and_apply_macros file_name =
  let string_map = ref StringMap.empty in
  let result_lines = ref [] in
  let process_line line =
    match parse_define_line line with
    | Some (identifier, value) -> string_map := StringMap.add identifier value !string_map
    | None ->
      let processed_line = apply_macros !string_map line in
      result_lines := processed_line :: !result_lines
  in
  let ic = open_in file_name in
  try
    while true do
      let line = input_line ic in
      process_line line
    done;
    close_in ic;
    List.rev !result_lines
  with
  | End_of_file ->
    close_in ic;
    List.rev !result_lines

let write_result_to_file file_name lines =
  let dot_index = String.rindex file_name '.' in
  let base_name = String.sub file_name 0 dot_index in
  let extension = String.sub file_name dot_index (String.length file_name - dot_index) in
  let new_file_name = base_name ^ "_macro" ^ extension in
  let oc = open_out new_file_name in
  List.iter (fun line -> output_string oc (if String.length line > 0 && line.[String.length line - 1] = ' ' then String.sub line 0 (String.length line - 1) else line); output_char oc '\n') lines;
  close_out oc

let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Usage: %s <input_file>\n" Sys.argv.(0)
  else begin
    let file_name = Sys.argv.(1) in
    let result_lines = populate_string_map_and_apply_macros file_name in
    write_result_to_file file_name result_lines;
  end

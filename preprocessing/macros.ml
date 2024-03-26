module MacroProcessor = struct
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
        process_line line;
      done;
      close_in ic;
      List.rev !result_lines
    with
    | End_of_file ->
      close_in ic;
      List.rev !result_lines

  let write_result_to_file file_name lines =
    let base_name = Filename.basename file_name in
    let dot_index = String.rindex base_name '.' in
    let base_name_without_extension = String.sub base_name 0 dot_index in
    let extension = String.sub base_name dot_index (String.length base_name - dot_index) in
    let new_file_name = "./temp_files/" ^ base_name_without_extension ^ "_m" ^ extension in
    let oc = open_out new_file_name in
    List.iter (fun line -> output_string oc (if String.length line > 0 && line.[String.length line - 1] = ' ' then String.sub line 0 (String.length line - 1) else line); output_char oc '\n') lines;
    close_out oc

  let process_file input_file =
    let result_lines = populate_string_map_and_apply_macros input_file in
    write_result_to_file input_file result_lines
end

module IndentFixup = struct
  let rec count_indent_helper string idx = 
    if idx >= String.length string then
      idx
    else
      if String.get string idx != ' ' then
        idx
      else
        count_indent_helper string (idx + 1)
  let is_space c = c=' '
  let count_indent string default= 
    if (String.for_all is_space string) then
      default
    else
    let r = count_indent_helper string 0 in
    if r mod 4 = 0 then r / 4
    else
      raise (Failure "Indentation Error")

  let rec string_repeat_helper str count acc =
    if count <= 0 then acc
    else string_repeat_helper str (count - 1) (acc ^ str)
  
  let string_repeat str count =
    string_repeat_helper str count ""
  let indent_fixup base_file_name =
    let input_file = "./temp_files/" ^ (Filename.remove_extension base_file_name) ^ "_m" ^ Filename.extension base_file_name in
    let result_lines = ref [] in
    let ic = open_in input_file in
    try
      let current_indents = ref 0 in
      while true do
        let line = input_line ic in
        let indents = count_indent line !current_indents in
        if (indents > !current_indents) then 
          let to_add = string_repeat "INDENT " (indents- !current_indents) in
          let processed_line = to_add ^ line in
          result_lines := processed_line :: !result_lines;
          current_indents := indents;
        else if (indents< !current_indents) then
          let to_add = string_repeat "DEDENT " (!current_indents-indents) in
          let processed_line = to_add ^ line in
          result_lines := processed_line :: !result_lines;
          current_indents := indents;
        else 
          result_lines := line :: !result_lines
      done;
      close_in ic;
      let new_file_name = "./temp_files/" ^ Filename.chop_extension base_file_name ^ "_i" ^ Filename.extension base_file_name in
      let oc = open_out new_file_name in
      List.iter (fun line -> output_string oc (line ^ "\n")) (List.rev !result_lines);
      close_out oc;
      new_file_name
    with
    | End_of_file ->
      close_in ic;
      let new_file_name = "./temp_files/" ^ Filename.chop_extension base_file_name ^ "_i" ^ Filename.extension base_file_name in
      let oc = open_out new_file_name in
      List.iter (fun line -> output_string oc (line ^ "\n")) (List.rev !result_lines);
      close_out oc;
      new_file_name
end

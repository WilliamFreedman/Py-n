let rec string_repeat n s =
  if n = 0 then "" else s ^ " " ^ string_repeat (n - 1) s

let should_ignore str = 
  ((String.for_all (fun x -> x = ' ')) str || ((String.length str) = 0))

let process_line define_map current_indent line line_num =
  let indent_diff = String.length line - String.length (String.trim line) in
  let indent_change = 
    if indent_diff > current_indent then
      if ((indent_diff - current_indent) mod 4) != 0 then raise (Failure ("Indentation error in line " ^ line)) else
      string_repeat ((indent_diff - current_indent)/4) "INDENT"
    else if indent_diff < current_indent then
      string_repeat ((current_indent - indent_diff)/4) "DEDENT"
    else
      "" in
  let updated_indent = indent_diff in
  let replace_x_with_y str (x, y) =
    Str.global_replace (Str.regexp_string x) y str in
  let line = List.fold_left replace_x_with_y line define_map in
  (indent_change, updated_indent, line)

  (* if a line is blank (i.e. nothing more than spaces), then we should avoid tracking indent changes *)

  let process_line define_map current_indent line line_num =
    
    let indent_diff = String.length line - String.length (String.trim line) in
    if should_ignore line then
      
    let indent_change = 
      if indent_diff > current_indent then
        if ((indent_diff - current_indent) mod 4) != 0 then raise (Failure ("Indentation error in line " ^ string_of_int !line_num ^ ": "^ line)) else
        string_repeat ((indent_diff - current_indent)/4) "INDENT"
      else if indent_diff < current_indent then
        (String.concat "\n" (Array.to_list (Array.make ((current_indent - indent_diff) / 4) "DEDENT ")) ) (*^ "\n"*)
      else
        "" in
    let updated_indent = indent_diff in
    let replace_x_with_y str (x, y) =
      Str.global_replace (Str.regexp_string x) y str in
    let line = List.fold_left replace_x_with_y line define_map in
    (indent_change, updated_indent, line)
    let preprocess_file input_file output_file =
      let define_map = ref [] in
      let line_num = ref 1 in
      let current_indent = ref 0 in
      let input_channel = open_in input_file in
      let output_channel = open_out output_file in
      try
        while true do
          ignore(!line_num = !line_num+1);
          let line = input_line input_channel in
          match String.split_on_char ' ' line with
          | ["@DEFINE"; x; y] -> define_map := (x, y) :: !define_map
          | _ ->
            let (indent_change, updated_indent, processed_line) =
              process_line !define_map !current_indent line line_num in
            output_string output_channel (indent_change ^ processed_line ^ "\n");
            current_indent := updated_indent
        done;
      with End_of_file ->
        close_in input_channel;
        close_out output_channel;
        let final_dedent = (String.concat "\n" (Array.to_list (Array.make (!current_indent / 4) "DEDENT"))^"\n") in
        output_string (open_out_gen [Open_append; Open_creat] 0o666 output_file) final_dedent


let () =
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  preprocess_file input_file output_file

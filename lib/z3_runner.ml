let run_output query =
  let file_name = Filename.temp_file "mini-liquid-" ".smt2" in
  let file = open_out file_name in

  output_string file query;
  close_out file;

  let process =
    Unix.open_process_in ("z3 " ^ Filename.quote file_name)
  in

  let rec read_lines collected =
    try
      let line = input_line process in
      read_lines (line :: collected)
    with End_of_file ->
      List.rev collected
  in

  let lines = read_lines [] in

  ignore (Unix.close_process_in process);
  Sys.remove file_name;

  lines

  let run query =
    match run_output query with
    | answer :: _ -> answer
    | [] -> failwith "Z3 produced no output"


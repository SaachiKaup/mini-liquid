let run query =
  let file_name = Filename.temp_file "mini-liquid-" ".smt2" in
  let file = open_out file_name in

  output_string file query;
  close_out file;

  let process =
    Unix.open_process_in ("z3 " ^ Filename.quote file_name)
  in
  let answer = input_line process in

  ignore (Unix.close_process_in process);
  Sys.remove file_name;

  answer

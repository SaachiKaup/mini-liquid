let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "usage: cmt_check SOURCE.ml";
    exit 2
  end;
  let source_path = Sys.argv.(1) in
  let loaded = Mini_liquid.Cmt_frontend.compile_and_read source_path in
  Printf.printf "Loaded %d top-level item(s) from %s\n"
    (List.length loaded.structure.str_items)
    source_path

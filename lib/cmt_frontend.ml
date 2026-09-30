open Typedtree

type typed_source = {
  structure : structure;
  initial_env : Env.t;
}

let compiler_command () =
  match Sys.getenv_opt "OCAMLC" with
  | Some command -> command
  | None -> "ocamlc"

let cmt_path source_path =
  Filename.remove_extension source_path ^ ".cmt"

let compile_to_cmt source_path =
  if not (Sys.file_exists source_path) then
    invalid_arg (Printf.sprintf "Source file does not exist: %s" source_path);
  let command = compiler_command () in
  let argv = [| command; "-bin-annot"; "-c"; source_path |] in
  let process_id = Unix.create_process command argv Unix.stdin Unix.stdout Unix.stderr in
  let rec wait_for_compiler () =
    try Unix.waitpid [] process_id with
    | Unix.Unix_error (Unix.EINTR, _, _) -> wait_for_compiler ()
  in
  let _, status = wait_for_compiler () in
  match status with
  | Unix.WEXITED 0 -> cmt_path source_path
  | Unix.WEXITED code ->
      failwith (Printf.sprintf "OCaml compiler exited with status %d" code)
  | Unix.WSIGNALED signal ->
      failwith (Printf.sprintf "OCaml compiler was terminated by signal %d" signal)
  | Unix.WSTOPPED signal ->
      failwith (Printf.sprintf "OCaml compiler stopped with signal %d" signal)

let read_cmt path =
  let cmt = Cmt_format.read_cmt path in
  match cmt.cmt_annots with
  | Implementation structure ->
      { structure; initial_env = cmt.cmt_initial_env }
  | _ ->
      failwith (Printf.sprintf "Expected implementation annotations in %s" path)

let compile_and_read source_path =
  read_cmt (compile_to_cmt source_path)

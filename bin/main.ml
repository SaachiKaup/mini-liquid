open Mini_liquid.Syntax
open Mini_liquid.Constraints
open Mini_liquid.Qualifiers
open Mini_liquid.Env_qualifiers
open Mini_liquid.Rule_engine
open Mini_liquid.Solver

let source_path = "lib/programs/max_program.ml"

let empty_context = {
  variables = [];
  guards = [];
}

let print_obligation obligation =
  print_endline
    ("  " ^ string_of_subtyping_obligation obligation)

let print_stage title =
  Printf.printf
    "\n\n========================================\n%s\n========================================\n\n"
    title

let print_substage number description =
  Printf.printf "\n  %s  %s\n\n" number description

let () =
  print_stage "1. SOURCE FRONTEND";
  print_substage "1.1" "Parse and typecheck the source program";
  Printf.printf "Reading: %s\n" source_path;
  print_endline "OCaml is producing a Typedtree and checking ordinary types.";
  let typed_source =
    Mini_liquid.Cmt_frontend.compile_and_read source_path
  in
  print_endline "OCaml ordinary typechecking: succeeded.";
  Printf.printf "Typedtree loaded from: %s\n"
    (Filename.remove_extension source_path ^ ".cmt");

  print_stage "2. TYPEDTREE CONVERSION";
  print_substage "2.1" "Convert the Typedtree to the Liquid expression AST";
  print_endline "The supported max expression is being converted.";
  let max_program =
    Mini_liquid.Typedtree_adapter.max_expression typed_source.structure
  in
  print_endline ("Converted program: " ^ string_of_expr max_program);

  print_stage "3. LIQUID INFERENCE";
  print_substage "3.1" "Traverse the converted expression";
  print_endline "Applying the Liquid typing rules to the function and its body.";
  let template_type, obligations =
    infer_expression empty_context max_program
  in

  print_substage "3.2" "Record the branch obligations";
  print_endline "The conditional creates one obligation for each branch:";
  List.iter print_obligation obligations;

  print_stage "4. QUALIFIER GENERATION";
  print_substage "4.1" "Instantiate qualifier templates";
  print_endline
    "Instantiating the qualifier templates with variables visible in each obligation.";
  let candidate_qualifiers =
    generated_for_obligations obligations
  in

  print_endline "Candidate qualifiers:";
  List.iter
    (fun candidate ->
      print_endline ("  " ^ string_of_fact candidate))
    candidate_qualifiers;

  print_stage "5. Z3 VALIDATION";
  print_substage "5.1" "Check each qualifier against every obligation";
  print_endline "Z3 checks whether each candidate is implied by every branch obligation.";
  let candidate_results =
    List.map
      (fun candidate ->
        Printf.printf "\nSending qualifier to Z3:\n  %s\n"
          (string_of_fact candidate);
        let result = counterexample_in_any_branch obligations candidate in
        begin match result with
        | None ->
            Printf.printf "Z3 result: keep %s\n"
              (string_of_fact candidate)
        | Some model ->
            Printf.printf "Z3 result: remove %s\n"
              (string_of_fact candidate);
            Printf.printf "Counterexample:\n%s\n" model
        end;
        (candidate, result))
      candidate_qualifiers
  in

  let surviving_qualifiers =
    List.filter_map
      (fun (candidate, result) ->
        match result with
        | None -> Some candidate
        | Some _ -> None)
      candidate_results
  in
  let final_type =
    fill_kappa "kappa0" surviving_qualifiers template_type
  in

  print_stage "6. FINAL REFINEMENT";
  print_substage "6.1" "Keep the qualifiers accepted by Z3";
  print_endline "Qualifiers that survived the Z3 checks:";
  print_endline
    ("  "
     ^ string_of_refinement_template
         (KnownFacts surviving_qualifiers));

  print_endline "Final inferred Liquid type for max:";
  print_endline ("  " ^ string_of_liquid_type_template final_type)

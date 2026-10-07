open Mini_liquid.Syntax
open Mini_liquid.Constraints
open Mini_liquid.Rule_engine
open Mini_liquid.Solver

let empty_context = {
  variables = [];
  guards = [];
}

let print_stage number title =
  Printf.printf "\n%s. %s\n%s\n"
    number
    title
    (String.make 72 '=')

let print_obligations obligations =
  List.iteri
    (fun index obligation ->
      Printf.printf "\n  Constraint %d\n    %s\n"
        (index + 1)
        (string_of_subtyping_obligation obligation))
    obligations

let print_assignment (kappa_name, facts) =
  Printf.printf "  %s := %s\n"
    kappa_name
    (string_of_refinement_template (KnownFacts facts))

let read_binding source_path binding_name =
  print_stage "1" "OCaml frontend";
  Printf.printf "Reading %s\n" source_path;
  let typed_source =
    Mini_liquid.Cmt_frontend.compile_and_read source_path
  in
  print_endline "Ordinary OCaml typechecking succeeded.";
  print_stage "2" "Typedtree to thin AST";
  let binding =
    Mini_liquid.Typedtree_adapter.binding_named
      binding_name
      typed_source.structure
  in
  Printf.printf "Binding: %s\n"
    (string_of_expr binding.definition_expression);
  binding

let run_sum () =
  let sum_binding =
    read_binding "lib/programs/sum_problem.ml" "sum"
  in
  print_stage "3" "Recursive Liquid-Type constraints";
  let recursive_template, obligations =
    infer_recursive_binding empty_context sum_binding
  in
  Printf.printf "Temporary recursive type:\n  %s\n"
    (string_of_liquid_type_template recursive_template);
  print_endline "\nGenerated constraints:";
  print_obligations obligations;

  (* Deliberately explicit candidates for the narrow sum demonstration.
     Automatic multi-kappa qualifier search is a later extension. *)
  let candidate_assignments = [
    ("sum_input", []);
    ("sum_output", [
      GreaterOrEqual (Name "result", Integer 0);
      GreaterOrEqual (Name "result", Name "k");
    ]);
  ] in

  print_stage "4" "Instantiate recursive refinements";
  print_endline "Candidate kappa assignments:";
  List.iter print_assignment candidate_assignments;
  let solved_obligations =
    List.map
      (fill_obligation_kappas candidate_assignments)
      obligations
  in
  print_endline "\nConstraints after pending substitutions are expanded:";
  print_obligations solved_obligations;

  print_stage "5" "Z3 validation";
  let results =
    List.map obligation_holds solved_obligations
  in
  List.iteri
    (fun index holds ->
      Printf.printf "  Constraint %d: %s\n"
        (index + 1)
        (if holds then "proved" else "not proved"))
    results;

  print_stage "6" "Final recursive Liquid type";
  let final_type =
    fill_kappas candidate_assignments recursive_template
  in
  Printf.printf "  %s\n" (string_of_liquid_type_template final_type)

let run_max () =
  let max_binding =
    read_binding "lib/programs/max_program.ml" "max"
  in
  print_stage "3" "Ordinary Liquid-Type constraints";
  let template_type, obligations =
    infer_expression empty_context max_binding.definition_expression
  in
  print_endline "Generated branch constraints:";
  print_obligations obligations;

  print_stage "4" "Qualifier generation";
  let candidates =
    Mini_liquid.Env_qualifiers.generated_for_obligations obligations
  in
  List.iter
    (fun fact -> Printf.printf "  %s\n" (string_of_fact fact))
    candidates;

  print_stage "5" "Z3 validation";
  let surviving =
    List.filter
      (fun candidate ->
        let holds = candidate_holds_for_all obligations candidate in
        Printf.printf "  %s: %s\n"
          (string_of_fact candidate)
          (if holds then "keep" else "remove");
        holds)
      candidates
  in

  print_stage "6" "Final Liquid type";
  let final_type =
    fill_kappa "kappa0" surviving template_type
  in
  Printf.printf "  %s\n" (string_of_liquid_type_template final_type)

let selected_mode () =
  match Array.to_list Sys.argv with
  | [_] -> "sum"
  | [_; mode] -> mode
  | _ ->
      failwith "Usage: dune exec ./bin/main.exe -- [sum|max]"

let () =
  match selected_mode () with
  | "sum" -> run_sum ()
  | "max" -> run_max ()
  | mode ->
      failwith ("Unknown example: " ^ mode ^ ". Choose sum or max.")

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

let print_weakening { Mini_liquid.Solver.kappa_name; removed } =
  Printf.printf "  %s: remove %s\n"
    kappa_name
    (string_of_refinement_template (KnownFacts removed))

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

let run_recursive_example example_name source_path binding_name =
  let recursive_binding =
    read_binding source_path binding_name
  in
  print_stage "3" "Recursive Liquid-Type constraints";
  let recursive_template, obligations =
    infer_recursive_binding empty_context recursive_binding
  in
  Printf.printf "Temporary recursive type:\n  %s\n"
    (string_of_liquid_type_template recursive_template);
  print_endline "\nGenerated constraints:";
  print_obligations obligations;

  let kappa_scopes =
    recursive_kappa_scopes empty_context recursive_binding
  in
  let initial_assignments, weakenings, assignments =
    solve_recursive_kappas kappa_scopes obligations
  in

  print_stage "4" "Initial Q* assignments";
  List.iter print_assignment initial_assignments;

  print_stage "5" "Iterative weakening";
  if weakenings = [] then
    print_endline "  No candidates were removed."
  else
    List.iter print_weakening weakenings;

  print_stage "6" "Z3 validation";
  let solved_obligations =
    List.map
      (fill_obligation_kappas assignments)
      obligations
  in
  let results =
    List.map obligation_holds solved_obligations
  in
  List.iteri
    (fun index holds ->
      Printf.printf "  Constraint %d: %s\n"
        (index + 1)
      (if holds then "proved" else "not proved"))
    results;

  print_stage "7" "Final recursive Liquid type";
  Printf.printf "Example: %s\n" example_name;
  print_endline "Stable kappa assignments:";
  List.iter print_assignment assignments;
  let final_type =
    fill_kappas assignments recursive_template
  in
  Printf.printf "  %s\n" (string_of_liquid_type_template final_type)

let run_regular_example example_name source_path binding_name =
  let binding =
    read_binding source_path binding_name
  in
  print_stage "3" "Ordinary Liquid-Type constraints";
  let template_type, obligations =
    infer_expression empty_context binding.definition_expression
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
  Printf.printf "Example: %s\n" example_name;
  let final_type =
    fill_kappa "kappa0" surviving template_type
  in
  Printf.printf "  %s\n" (string_of_liquid_type_template final_type)

let selected_mode () =
  match Array.to_list Sys.argv with
  | [_] -> "sum"
  | [_; mode] -> mode
  | _ ->
      failwith "Usage: dune exec ./bin/main.exe -- [max|voting|grade|sum|fibonacci]"

let () =
  match selected_mode () with
  | "max" -> run_regular_example "max" "examples/max.ml" "max"
  | "voting" ->
      run_regular_example "voting eligibility" "examples/voting.ml" "voting"
  | "grade" ->
      run_regular_example "pass/fail grade" "examples/grade.ml" "grade"
  | "sum" -> run_recursive_example "sum" "examples/sum.ml" "sum"
  | "fibonacci" ->
      run_recursive_example "fibonacci" "examples/fibonacci.ml" "fibonacci"
  | mode ->
      failwith ("Unknown example: " ^ mode ^ ". Choose max, voting, grade, sum, or fibonacci.")

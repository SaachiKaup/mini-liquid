open Mini_liquid.Syntax
open Mini_liquid.Constraints
open Mini_liquid.Rule_engine
open Mini_liquid.Solver

let source_path = "lib/programs/sum_problem.ml"

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

let () =
  print_stage "1" "OCaml frontend";
  Printf.printf "Reading %s\n" source_path;
  let typed_source =
    Mini_liquid.Cmt_frontend.compile_and_read source_path
  in
  print_endline "Ordinary OCaml typechecking succeeded.";

  print_stage "2" "Typedtree to thin AST";
  let sum_binding =
    Mini_liquid.Typedtree_adapter.binding_named "sum" typed_source.structure
  in
  Printf.printf "Recursive binding: %s\n"
    (string_of_expr sum_binding.definition_expression);

  print_stage "3" "Recursive Liquid-Type constraints";
  let recursive_template, obligations =
    infer_recursive_binding empty_context sum_binding
  in
  Printf.printf "Temporary recursive type:\n  %s\n"
    (string_of_liquid_type_template recursive_template);
  print_endline "\nGenerated constraints:";
  print_obligations obligations;

  (* This is a deliberately explicit candidate assignment for the narrow
     sum example. Automatic multi-kappa qualifier search is a later step. *)
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

open Mini_liquid.Syntax
open Mini_liquid.Constraints
open Mini_liquid.Qualifiers
open Mini_liquid.Rule_engine
open Mini_liquid.Solver

let max_program =
  Function (
    "x",
    IntType,
    Function (
      "y",
      IntType,
      If (
        GreaterThan (Variable "x", Variable "y"),
        Variable "x",
        Variable "y"
      )
    )
  )

let empty_context = {
  variables = [];
  guards = [];
}

let print_obligation obligation =
  print_endline
    ("  " ^ string_of_subtyping_obligation obligation)

let () =
  let template_type, obligations =
    infer_expression empty_context max_program
  in

  print_endline "Program:";
  print_endline ("  " ^ string_of_expr max_program);

  print_endline "\nLT-IF generated obligations:";
  List.iter print_obligation obligations;

  print_endline "\nCandidate qualifiers Q:";
  List.iter
    (fun candidate ->
      print_endline ("  " ^ string_of_fact candidate))
    max_qualifiers;

  print_endline "\nZ3 checks:";
  let candidate_results =
    List.map
      (fun candidate ->
        (candidate, counterexample_for_candidate obligations candidate))
      max_qualifiers
  in
  List.iter
    (fun (candidate, result) ->
      match result with
      | None ->
          print_endline ("  keep: " ^ string_of_fact candidate)
      | Some model ->
          print_endline ("  remove: " ^ string_of_fact candidate);
          print_endline ("    counterexample:\n" ^ model))
    candidate_results;

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

  print_endline "\nFinal meaning of kappa0:";
  print_endline
    ("  "
     ^ string_of_refinement_template
         (KnownFacts surviving_qualifiers));

  print_endline "\nFinal inferred type for max:";
  print_endline ("  " ^ string_of_liquid_type_template final_type)

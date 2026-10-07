open Syntax
open Constraints
open Smtlib

let candidate_holds_for_obligation obligation candidate =
  let known_facts = facts_of_obligation obligation in
  let query = max_query known_facts candidate in
  Z3_runner.run query = "unsat"

let candidate_holds_for_all obligations candidate =
  List.for_all
    (fun obligation ->
      candidate_holds_for_obligation obligation candidate)
    obligations

let counterexample_in_one_branch obligation candidate =
  let known_facts = facts_of_obligation obligation in
  let query = max_query known_facts candidate in

  match Z3_runner.run query with
  | "unsat" ->
      None

  | "sat" ->
      let model_query =
        max_query_with_model known_facts candidate
      in
      (
        match Z3_runner.run_output model_query with
        | _answer :: model_lines ->
            Some (String.concat "\n" model_lines)
        | [] ->
            failwith "Z3 produced no model"
      )

  | answer ->
      failwith ("Unexpected Z3 answer: " ^ answer)

let rec search_for_counterexample candidate = function
  | [] ->
      None
  | obligation :: remaining ->
      match counterexample_in_one_branch obligation candidate with
      | Some model -> Some model
      | None -> search_for_counterexample candidate remaining

let counterexample_in_any_branch obligations candidate =
  search_for_counterexample candidate obligations

let rec fill_kappa kappa_name facts = function
  | BaseLiquidType (
      base_type,
      UnknownRefinement { kappa_name = Kappa name; pending_substitutions }
    )
    when name = kappa_name ->
      BaseLiquidType (
        base_type,
        KnownFacts (apply_pending_substitutions pending_substitutions facts)
      )
  | BaseLiquidType _ as liquid_type ->
      liquid_type
  | FunctionLiquidType (name, input_type, output_type) ->
      FunctionLiquidType (
        name,
        fill_kappa kappa_name facts input_type,
        fill_kappa kappa_name facts output_type
      )

let fill_kappas assignments liquid_type =
  List.fold_left
    (fun liquid_type (kappa_name, facts) ->
      fill_kappa kappa_name facts liquid_type)
    liquid_type
    assignments

let fill_obligation_kappas assignments obligation =
  let fill = fill_kappas assignments in
  {
    context = {
      obligation.context with
      variables =
        List.map
          (fun (name, liquid_type) -> (name, fill liquid_type))
          obligation.context.variables;
    };
    actual_type = fill obligation.actual_type;
    expected_type = fill obligation.expected_type;
  }

let obligation_holds obligation =
  let assumptions = facts_of_obligation obligation in
  let expected_facts = expected_facts_of_obligation obligation in
  Z3_runner.run (subtyping_query assumptions expected_facts) = "unsat"

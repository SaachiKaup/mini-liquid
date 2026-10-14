open Syntax
open Constraints
open Smtlib

type kappa_assignment = string * fact list

type weakening = {
  kappa_name : string;
  removed : fact list;
}

let z3_proves query =
  match Z3_runner.run query with
  | "unsat" -> true
  | "sat" -> false
  | answer -> failwith ("Unexpected Z3 answer: " ^ answer)

let candidate_holds_for_obligation obligation candidate =
  let known_facts = facts_of_obligation obligation in
  let query = subtyping_query known_facts [candidate] in
  z3_proves query

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
  z3_proves (subtyping_query assumptions expected_facts)

let initial_kappa_assignments kappa_scopes : kappa_assignment list =
  List.map
    (fun (kappa_name, scope) ->
      (kappa_name, Qualifiers.generated_qualifiers scope))
    kappa_scopes

let target_kappa_name obligation =
  match obligation.expected_type with
  | BaseLiquidType (_, UnknownRefinement { kappa_name = Kappa name; _ }) ->
      name
  | _ ->
      failwith "A recursive solver obligation must target an unknown base refinement"

let expected_type_for_candidate obligation candidate =
  match obligation.expected_type with
  | BaseLiquidType
      (base_type, UnknownRefinement { pending_substitutions; _ }) ->
      BaseLiquidType
        (base_type, KnownFacts (apply_pending_substitutions pending_substitutions [candidate]))
  | _ ->
      failwith "A recursive solver obligation must target an unknown base refinement"

(* Fill the facts that are available from every current kappa assignment, but
   check one candidate fact for the kappa named by this obligation. *)
let candidate_holds_for_recursive_obligation assignments obligation candidate =
  let filled = fill_obligation_kappas assignments obligation in
  let candidate_obligation =
    { filled with expected_type = expected_type_for_candidate obligation candidate }
  in
  obligation_holds candidate_obligation

let refined_facts assignments kappa_name facts obligations =
  let target_obligations =
    List.filter (fun obligation -> target_kappa_name obligation = kappa_name) obligations
  in
  List.filter
    (fun candidate ->
      List.for_all
        (fun obligation ->
          candidate_holds_for_recursive_obligation assignments obligation candidate)
        target_obligations)
    facts

(* Start from Q* and repeatedly weaken the first assignment whose candidates
   are not justified.  Restarting after every weakening is the small, direct
   fixed-point strategy used for this project. *)
let solve_recursive_kappas kappa_scopes obligations =
  let initial = initial_kappa_assignments kappa_scopes in
  let rec stabilize assignments steps =
    let rec weaken_first prefix = function
      | [] -> None
      | (kappa_name, facts) :: remaining ->
          let refined = refined_facts assignments kappa_name facts obligations in
          if refined = facts then
            weaken_first ((kappa_name, facts) :: prefix) remaining
          else
            let next_assignments = List.rev_append prefix ((kappa_name, refined) :: remaining) in
            let removed = List.filter (fun fact -> not (List.mem fact refined)) facts in
            Some (next_assignments, { kappa_name; removed })
    in
    match weaken_first [] assignments with
    | None -> (initial, List.rev steps, assignments)
    | Some (next_assignments, step) ->
        stabilize next_assignments (step :: steps)
  in
  stabilize initial []

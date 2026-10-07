open Syntax

(* Environment is effectively a small lookup table 
  [
   "x" → {result : int | true},
   "y" → {result : int | true}
  ]
*)
type environment =
  (string * liquid_type_template) list

(* the context 'world' as he calls it in the video, has both variables and facts or guards' *)
type context = {
  variables : environment;
  guards : fact list;
}

type subtyping_obligation = {
  context : context;
  actual_type : liquid_type_template;
  expected_type : liquid_type_template;
}

let facts_of_base_type = function
  | BaseLiquidType (_, KnownFacts actual_facts) ->
      actual_facts
  | _ ->
      failwith "Expected a base type with known facts"

let facts_of_binding (name, liquid_type) =
  match liquid_type with
  | BaseLiquidType (_, KnownFacts facts) ->
      List.map (substitute_fact "result" (Name name)) facts
  | BaseLiquidType (_, UnknownRefinement _) ->
      failwith "Cannot send an unfilled refinement kappa to the solver"
  | FunctionLiquidType _ ->
      []

let facts_of_context context =
  context.guards
  @ List.concat_map facts_of_binding context.variables

let facts_of_obligation obligation =
  facts_of_context obligation.context
  @ facts_of_base_type obligation.actual_type

let expected_facts_of_obligation obligation =
  facts_of_base_type obligation.expected_type

let string_of_binding (name, type_template) =
  Printf.sprintf "%s : %s"
    name
    (string_of_liquid_type_template type_template)

let string_of_context context =
  let variables =
    String.concat "; "
      (List.map string_of_binding context.variables)
  in
  let guards =
    String.concat " AND "
      (List.map string_of_fact context.guards)
  in
  Printf.sprintf "%s; %s" variables guards

let string_of_subtyping_obligation obligation =
  Printf.sprintf "%s ⊢ %s <: %s"
    (string_of_context obligation.context)
    (string_of_liquid_type_template obligation.actual_type)
    (string_of_liquid_type_template obligation.expected_type)

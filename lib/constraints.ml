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

let facts_of_obligation obligation =
  match obligation.actual_type with
  | BaseLiquidType (_, KnownFacts actual_facts) ->
      obligation.context.guards @ actual_facts
  | _ ->
      failwith "Expected a base type with known facts"

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

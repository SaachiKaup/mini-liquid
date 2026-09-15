open Syntax
open Constraints

(* paper rule

   Γ(x) = {ν : B | e}
   ────────────────────────  [LT-VAR]
   Γ ⊢Q x : {ν : B | ν = x}

which means if x is of a base type in the current environment then - 
   {result : int | result = x}
*)
let infer_variable context name = 
  match List.assoc_opt name context.variables with
  | Some (BaseLiquidType (base_type, _)) ->
      (
        BaseLiquidType (
            base_type,
            KnownFacts [Equal (Name "result", Name name)]
        ),
        []
      )
  | Some _ ->
      failwith "LT-VAR supports only base-type variables for now"
  | None ->
      failwith ("Unknown variable: " ^ name)

(* if condition helper *)
let fact_of_condition = function
  | GreaterThan (Variable left, Variable right) ->
      FactGreaterThan (Name left, Name right)
  | _ ->
      failwith "Only variable greater-than conditions are supported for now"

let add_guard context guard =
  { context with guards = guard :: context.guards }

let make_obligation context actual_type expected_type = {
  context;
  actual_type;
  expected_type;
}

let add_variable context name liquid_type =
  {
    context with
    variables = (name, liquid_type) :: context.variables;
  }

(* dispatcher which goes through program and picks which rule to apply *)
let rec infer_expression context expression =
  match expression with
  | Variable name ->
      infer_variable context name
  | Function (name, base_type, body) ->
      let input_param_type = 
        BaseLiquidType (base_type, KnownFacts [])
      in
      let body_type, obligations =
        infer_expression 
          (add_variable context name input_param_type)
          body
      in
      (FunctionLiquidType (name, input_param_type, body_type), obligations)
  | If (condition, then_branch, else_branch) ->
      let guard = fact_of_condition condition
      in
      let then_context = add_guard context guard
      in
      let else_context = add_guard context (Not guard)
      in
      let then_type, then_constraints =
        infer_expression then_context then_branch
      in
      let else_type, else_constraints =
        infer_expression else_context else_branch
      in
      let whole_type =
        BaseLiquidType (
          IntType,
          UnknownRefinement (Kappa "kappa0")
        )
      in
      (
        whole_type,
        [
          make_obligation then_context then_type whole_type;
          make_obligation else_context else_type whole_type;
        ]
        @ then_constraints
        @ else_constraints
      ) 
  | _ ->
      failwith ("Unknown act")

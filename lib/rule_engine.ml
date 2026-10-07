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
  | Apply (
      Variable "<",
      [Variable variable_name;
       Constant { compiler_constant = Asttypes.Const_int number }]
    ) ->
      (* [k < n] is represented using the existing greater-than fact as
         [n > k]. *)
      FactGreaterThan (Integer number, Name variable_name)
  | _ ->
      failwith "Only variable greater-than and variable-less-than-integer conditions are supported for now"

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

(* [LT-INT]: an integer literal has the precise refinement saying that its
   result is that integer. *)
let infer_constant literal =
  match literal.compiler_constant with
  | Asttypes.Const_int number ->
      (
        BaseLiquidType (
          OcamlType Predef.type_int,
          KnownFacts [Equal (Name "result", Integer number)]
        ),
        []
      )
  | _ ->
      failwith "Only integer constants are supported for now"

(* Translate the simple integer expressions supported by this project into
   refinement-language terms.  These terms can later be used in facts and in
   substitutions such as [k - 1 / k]. *)
let rec term_of_expression = function
  | Variable name ->
      Name name
  | Constant { compiler_constant = Asttypes.Const_int number } ->
      Integer number
  | Apply (Variable "+", [left; right]) ->
      Add (term_of_expression left, term_of_expression right)
  | Apply (Variable "-", [left; right]) ->
      Sub (term_of_expression left, term_of_expression right)
  | _ ->
      failwith "Only integer variables, constants, addition, and subtraction can become refinement terms"

(* dispatcher which goes through program and picks which rule to apply *)
let rec infer_expression context expression =
  match expression with
  | Variable name ->
      infer_variable context name
  | Constant literal ->
      infer_constant literal
  | Apply (Variable "+", [left; right]) ->
      infer_arithmetic context
        (Add (term_of_expression left, term_of_expression right))
        left
        right
  | Apply (Variable "-", [left; right]) ->
      infer_arithmetic context
        (Sub (term_of_expression left, term_of_expression right))
        left
        right
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
  | Let (binding, body) ->
      infer_local_let context binding body
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
        match then_type with
        | BaseLiquidType (then_base_type, _) -> BaseLiquidType (
            then_base_type, 
            UnknownRefinement {kappa_name = (Kappa "kappa0"); pending_substitutions = []}
          )
        | _ -> failwith "If branches must have base types" 
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

and infer_local_let context binding body =
  match binding with
  | { name; recursive_status = false; definition_expression } ->
      let definition_type, definition_obligations =
        infer_expression context definition_expression
      in
      let context_with_local =
        add_variable context name definition_type
      in
      let body_type, body_obligations =
        infer_expression context_with_local body
      in
      (body_type, definition_obligations @ body_obligations)
  | { recursive_status = true; _ } ->
      failwith "Recursive local lets must be inferred through the recursive-binding rule"

and infer_arithmetic context result_term left right =
  let left_type, left_obligations =
    infer_expression context left
  in
  let right_type, right_obligations =
    infer_expression context right
  in
  match left_type, right_type with
  | BaseLiquidType (base_type, _), BaseLiquidType _ ->
      (
        BaseLiquidType (
          base_type,
          KnownFacts [Equal (Name "result", result_term)]
        ),
        left_obligations @ right_obligations
      )
  | _ ->
      failwith "Arithmetic currently requires base-type operands"


let unfinished_template binding = 
  match binding with
  | { name = function_name; recursive_status = true;
      definition_expression = Function (parameter_name, parameter_base_type, _) } ->
      let input_type =
        BaseLiquidType (
          parameter_base_type,
          UnknownRefinement {
            kappa_name = Kappa (function_name ^ "_input");
            pending_substitutions = [];
          }
        )
      in
      let output_type =
        BaseLiquidType (
          parameter_base_type,
          UnknownRefinement {
            kappa_name = Kappa (function_name ^ "_output");
            pending_substitutions = [];
          }
        )
      in
      FunctionLiquidType (parameter_name, input_type, output_type)
  | { recursive_status = false; _ } ->
      failwith "A recursive template requires a let rec binding"
  | { recursive_status = true; _ } ->
      failwith "A recursive template requires a function definition"

(* Give a recursive definition its tentative liquid type while its body is
   checked.  This makes recursive uses of the function name resolvable. *)
let add_recursive_binding context binding =
  let recursive_template = unfinished_template binding in
  add_variable context binding.name recursive_template

let infer_recursive_binding context binding =
  let context_with_recursive_function =
    add_recursive_binding context binding
  in
  infer_expression context_with_recursive_function binding.definition_expression

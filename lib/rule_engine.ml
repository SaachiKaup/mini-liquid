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


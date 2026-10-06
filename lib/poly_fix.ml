open Syntax

type poly_var =
    | PolyVar of string
    | ConcreteType of liquid_type_template

type inner_function = 
    | Function of (poly_var * poly_var)

type function_type = {
    inner: inner_function;
    outer: poly_var
}

(* can be ThereExists also, but fine , ignoring for now *)
type fix_scheme_type =
    | ForAll of poly_var * function_type

let alpha = PolyVar "alpha"

let fix_scheme = 
    ForAll (
      alpha,
      {
        inner = Function (alpha, alpha);
        outer = alpha
      }
    )

let instantiate_fix replacement_type fix_scheme =
   match fix_scheme with
   | ForAll (_, _) -> {
       inner = Function (ConcreteType replacement_type, ConcreteType replacement_type);
       outer = ConcreteType replacement_type
     }

let string_of_poly_var = function
  | PolyVar name -> name
  | ConcreteType (FunctionLiquidType _ as liquid_type) ->
      Printf.sprintf "(%s)" (string_of_liquid_type_template liquid_type)
  | ConcreteType liquid_type -> string_of_liquid_type_template liquid_type

let string_of_inner_function = function
  | Function (left, right) ->
      Printf.sprintf "%s -> %s"
        (string_of_poly_var left)
        (string_of_poly_var right)

let string_of_function_type function_type =
  Printf.sprintf "(%s) -> %s"
    (string_of_inner_function function_type.inner)
    (string_of_poly_var function_type.outer)

let string_of_fix_scheme_type = function
  | ForAll (bound_variable, body) ->
      Printf.sprintf "forall %s. %s"
        (string_of_poly_var bound_variable)
        (string_of_function_type body)
    

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
    

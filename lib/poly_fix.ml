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

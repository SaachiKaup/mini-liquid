open Syntax

type qualifier_term = {
    result: Name,
    placeholder: Name, 
    constant: Integer
}

(* Set Q, we are manually providing, this is not derived, it is in the paper as something provided in the beginnning *)
let max_qualifiers = 
  [
    GreaterOrEqual (Name "result", Name "x");
    GreaterOrEqual (Name "result", Name "y");
    GreaterOrEqual (Name "result", Integer 50);
    FactGreaterThan (Name "result", Integer (-3));
  ]




open Syntax

type qualifier_term  = 
    | Result
    | Var of string 
    | Constant of int


(*
We only need <= and <
*)
type qualifier_comparison =
    | LessThanEqualTo
    | LessThan

type qualifier = {
    left_term: qualifier_term,
    relationship: qualifier_comparison,
    right_term: qualifier_term
}

(* Set Q, we are manually providing, this is not derived, it is in the paper as something provided in the beginnning *)
let max_qualifiers = 
  [
    GreaterOrEqual (Name "result", Name "x");
    GreaterOrEqual (Name "result", Name "y");
    GreaterOrEqual (Name "result", Integer 50);
    FactGreaterThan (Name "result", Integer (-3));
  ]




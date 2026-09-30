open Syntax
open Constraints

type qualifier_term  = 
    | Result
    | PlaceholderVar of string 
    | Constant of int


(*
We only need <= and <
*)
type qualifier_comparison =
    | LessThanEqualTo
    | LessThan

type qualifier = {
    left_term: qualifier_term;
    relationship: qualifier_comparison;
    right_term: qualifier_term
}

(* including all qualifiers *)
let qualifiers = 
  [
    {
      left_term = PlaceholderVar "A0";
      relationship = LessThanEqualTo;
      right_term = Result;
    };
    {
      left_term = Result;
      relationship = LessThan;
      right_term = PlaceholderVar "A0";
    };
    {
      left_term = Constant 0;
      relationship = LessThanEqualTo;
      right_term = Result;
    }
  ]

(*
let string_of_qualifier q =
    Printf.sprintf "%s %s %s"
      q.left_term q.relationship q.right_term
*)

open Syntax
open Constraints
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

(* The first resolution step: keep only names whose binding has base type int.
   Placeholder filling happens in a later function. *)
let integer_variables (env : environment) : string list =
  List.fold_right
    (fun (name, liquid_type) names ->
      match liquid_type with
      | BaseLiquidType (IntType, _) -> name :: names
      | _ -> names)
    env
    []

(* Return each placeholder label used by one qualifier, once and in source
   order. *)
let placeholder_names (q : qualifier) : string list =
  List.fold_right
    (fun term names ->
      match term with
      | PlaceholderVar name when not (List.mem name names) -> name :: names
      | _ -> names)
    [ q.left_term; q.right_term ]
    []

(* Produce every assignment of eligible variable names to placeholder labels. *)
let placeholder_assignments
    (placeholders : string list)
    (variables : string list) : (string * string) list list =
  List.fold_right
    (fun placeholder assignments ->
      List.concat
        (List.map
           (fun variable ->
             List.map
               (fun assignment -> (placeholder, variable) :: assignment)
               assignments)
           variables))
    placeholders
    [ [] ]

(*
let string_of_qualifier q =
    Printf.sprintf "%s %s %s"
      q.left_term q.relationship q.right_term
*)

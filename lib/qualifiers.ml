open Syntax
open Constraints

(* The current qualifier family is arithmetic, so it can only be instantiated
   with variables whose compiler type is OCaml's int. This is a boundary check
   for this qualifier family, not project-level type inference. *)
let is_integer_type = function
  | OcamlType type_expr ->
      begin match Types.get_desc type_expr with
      | Tconstr (path, [], _) -> Path.last path = "int"
      | _ -> false
      end

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
let eligible_variables (env : environment) : string list =
  List.fold_right
    (fun (name, liquid_type) names ->
      match liquid_type with
      | BaseLiquidType (base_type, _)
        when is_integer_type base_type -> name :: names
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

(* Fill one qualifier with one placeholder assignment and translate it to the
   concrete fact representation used by the solver. *)
let fill_qualifier
    (assignment : (string * string) list)
    (q : qualifier) : fact =
  let concrete_term = function
    | Result -> Name "result"
    | PlaceholderVar name ->
        Name (List.assoc name assignment)
    | Constant number -> Integer number
  in
  let left = concrete_term q.left_term in
  let right = concrete_term q.right_term in
  match q.relationship with
  | LessThanEqualTo -> GreaterOrEqual (right, left)
  | LessThan -> FactGreaterThan (right, left)

(* Instantiate every supplied qualifier using the visible integer variables. *)
let generated_qualifiers (env : environment) : fact list =
  let variables = eligible_variables env in
  let add_if_new facts assignment qualifier =
    let fact = fill_qualifier assignment qualifier in
    if List.mem fact facts then facts else fact :: facts
  in
  List.fold_right
    (fun qualifier facts ->
      let names = placeholder_names qualifier in
      let assignments = placeholder_assignments names variables in
      List.fold_right
        (fun assignment facts -> add_if_new facts assignment qualifier)
        assignments
        facts)
    qualifiers
    []

(*
let string_of_qualifier q =
    Printf.sprintf "%s %s %s"
      q.left_term q.relationship q.right_term
*)

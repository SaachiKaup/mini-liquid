open Syntax

let rec smt_of_term = function
  | Name name -> name
  | Integer number -> string_of_int number
  | Add (left, right) ->
      Printf.sprintf "(+ %s %s)" (smt_of_term left) (smt_of_term right)
  | Sub (left, right) ->
      Printf.sprintf "(- %s %s)" (smt_of_term left) (smt_of_term right)

let rec smt_of_fact = function
  | Equal (left, right) ->
      Printf.sprintf "(= %s %s)"
        (smt_of_term left)
        (smt_of_term right)
  | FactGreaterThan (left, right) ->
      Printf.sprintf "(> %s %s)"
        (smt_of_term left)
        (smt_of_term right)
  | GreaterOrEqual (left, right) ->
      Printf.sprintf "(>= %s %s)"
        (smt_of_term left)
        (smt_of_term right)
  | Not fact ->
      Printf.sprintf "(not %s)"
        (smt_of_fact fact)

let assertion fact =
  "(assert " ^ smt_of_fact fact ^ ")"

let rec names_of_term = function
  | Name name -> [name]
  | Integer _ -> []
  | Add (left, right)
  | Sub (left, right) ->
      names_of_term left @ names_of_term right

let rec names_of_fact = function
  | Equal (left, right)
  | FactGreaterThan (left, right)
  | GreaterOrEqual (left, right) ->
      names_of_term left @ names_of_term right
  | Not fact ->
      names_of_fact fact

let declarations_for_facts facts =
  facts
  |> List.concat_map names_of_fact
  |> List.sort_uniq String.compare
  |> List.map (fun name -> Printf.sprintf "(declare-const %s Int)" name)

let conjunction facts =
  match facts with
  | [] -> "true"
  | [fact] -> smt_of_fact fact
  | _ ->
      Printf.sprintf "(and %s)"
        (String.concat " " (List.map smt_of_fact facts))

(* Counterexample query for a concrete subtyping check:
   assumptions /\ not(expected).  [unsat] means the subtype check holds. *)
let subtyping_query assumptions expected_facts =
  let all_facts = assumptions @ expected_facts in
  String.concat "\n"
    (
      declarations_for_facts all_facts
      @ List.map assertion assumptions
      @ [
          Printf.sprintf "(assert (not %s))" (conjunction expected_facts);
          "(check-sat)";
        ]
    )

let max_query known_facts candidate =
  let declarations =
    [
      "(declare-const x Int)";
      "(declare-const y Int)";
      "(declare-const result Int)";
    ]
  in
  let counterexample = Not candidate in
  String.concat "\n"
    (
      declarations
      @ List.map assertion known_facts
      @
      [
        assertion counterexample;
        "(check-sat)";
      ]
    )

let max_query_with_model known_facts candidate =
  max_query known_facts candidate ^ "\n(get-model)"

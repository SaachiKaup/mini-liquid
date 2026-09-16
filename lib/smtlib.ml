open Syntax

let smt_of_term = function
  | Name name -> name
  | Integer number -> string_of_int number

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


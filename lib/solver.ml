open Syntax
open Constraints
open Smtlib

let candidate_holds_for_obligation obligation candidate =
  let known_facts = facts_of_obligation obligation in
  let query = max_query known_facts candidate in
  Z3_runner.run query = "unsat"

let candidate_holds_for_all obligations candidate =
  List.for_all
    (fun obligation ->
      candidate_holds_for_obligation obligation candidate)
    obligations

let counterexample_for_obligation obligation candidate =
  let known_facts = facts_of_obligation obligation in
  let query = max_query known_facts candidate in

  match Z3_runner.run query with
  | "unsat" ->
      None

  | "sat" ->
      let model_query =
        max_query_with_model known_facts candidate
      in
      (
        match Z3_runner.run_output model_query with
        | _answer :: model_lines ->
            Some (String.concat "\n" model_lines)
        | [] ->
            failwith "Z3 produced no model"
      )

  | answer ->
      failwith ("Unexpected Z3 answer: " ^ answer)

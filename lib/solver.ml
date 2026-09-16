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

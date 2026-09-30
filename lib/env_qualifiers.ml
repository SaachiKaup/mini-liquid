open Syntax
open Constraints
open Qualifiers

(* The current rule-engine milestone creates one obligation for each branch of
   an if-expression. Both branches should see the same function-variable
   environment; only their guards differ. *)
let shared_environment (obligations : subtyping_obligation list) : environment =
  match obligations with
  | [ first; second ] ->
      if first.context.variables = second.context.variables then
        first.context.variables
      else
        failwith "Branch obligations have different variable environments"
  | _ ->
      failwith "Expected exactly two branch obligations"

(* Generate concrete candidate facts for the shared branch environment. *)
let generated_for_obligations
    (obligations : subtyping_obligation list) : fact list =
  generated_qualifiers (shared_environment obligations)

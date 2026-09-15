open Syntax

(* Environment is effectively a small lookup table 
  [
   "x" → {result : int | true},
   "y" → {result : int | true}
  ]
*)
type environment =
  (string * liquid_type_template) list

(* the context 'world' as he calls it in the video, has both variables and facts or guards' *)
type context = {
  variables : environment;
  guards : fact list;
}

type subtyping_obligation = {
  context : context;
  actual_type : liquid_type_template;
  expected_type : liquid_type_template;
}

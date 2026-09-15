open Mini_liquid.Syntax

let max_program =
  Function (
    "x",
    IntType,
    Function (
      "y",
      IntType,
      If (
        GreaterThan (Variable "x", Variable "y"),
        Variable "x",
        Variable "y"
      )
    )
  )

let true_branch_facts =
  [
    FactGreaterThan (Name "x", Name "y");
    Equal (Name "result", Name "x");
  ]

let false_branch_facts =
  [
    Not (FactGreaterThan (Name "x", Name "y"));
    Equal (Name "result", Name "y");
  ]

let max_result_refinement = UnknownRefinement (Kappa "kappa0")

let () =
  print_endline (string_of_expr max_program);
  print_endline "True branch:";
  List.iter
    (fun fact -> print_endline ("  " ^ string_of_fact fact))
    true_branch_facts;
  print_endline "False branch:";
  List.iter
    (fun fact -> print_endline ("  " ^ string_of_fact fact))
    false_branch_facts;
  print_endline ("Whole-if result refinement: " ^ string_of_refinement_template max_result_refinement)

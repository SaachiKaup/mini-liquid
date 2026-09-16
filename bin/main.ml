open Mini_liquid.Syntax
open Mini_liquid.Constraints
open Mini_liquid.Rule_engine

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
let max_result_type = BaseLiquidType (IntType, max_result_refinement)

let unrestricted_int = BaseLiquidType (IntType, KnownFacts [])

let true_context = {
  variables = 
    [
      ("x", unrestricted_int);
      ("y", unrestricted_int);
    ];
  guards = 
    [
      FactGreaterThan (Name "x", Name "y");
    ]
}

let false_context = {
  variables =
    [
      ("x", unrestricted_int);
      ("y", unrestricted_int);
    ];
  guards =
    [
      Not (FactGreaterThan (Name "x", Name "y"));
    ];
}

let x_result_type = 
  BaseLiquidType (
    IntType,
    KnownFacts [Equal (Name "result", Name "x")]
  )

let y_result_type =
  BaseLiquidType (
    IntType,
    KnownFacts [Equal (Name "result", Name "y")]
  )

let true_branch_obligation = {
  context = true_context;
  actual_type = x_result_type;
  expected_type = max_result_type;
}

let false_branch_obligation = {
  context = false_context;
  actual_type = y_result_type;
  expected_type = max_result_type;
}

let inferred_x_type, obligations = 
    infer_variable true_context "x"

let identity_program =
  Function (
    "x",
    IntType,
    Variable "x"
  )

let empty_context = {
  variables = [];
  guards = [];
}

let identity_type, identity_obligations =
  infer_expression empty_context identity_program

let inferred_max_type, inferred_max_obligations =
  infer_expression empty_context max_program

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
  print_endline ("Whole-if result refinement: " ^ string_of_refinement_template max_result_refinement);
  print_endline ("True-branch obligation: \n " ^ string_of_subtyping_obligation true_branch_obligation);
  print_endline ("False-branch obligation: \n " ^ string_of_subtyping_obligation false_branch_obligation);
  print_endline
    ("LT-VAR inferred for x: "
     ^ string_of_liquid_type_template inferred_x_type);
  print_endline
    ("LT-FUN inferred for identity: "
     ^ string_of_liquid_type_template identity_type);
  print_endline
      ("LT-IF inferred for max: "
       ^ string_of_liquid_type_template inferred_max_type);
  print_endline "LT-IF generated obligations:";
  List.iter
    (fun obligation ->
      print_endline
        ("  " ^ string_of_subtyping_obligation obligation))
    inferred_max_obligations;

  print_endline "Facts collected from generated obligations:";
  List.iter
    (fun obligation ->
      List.iter
        (fun fact -> print_endline ("  " ^ string_of_fact fact))
        (facts_of_obligation obligation))
    inferred_max_obligations;


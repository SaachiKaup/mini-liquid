open Mini_liquid.Syntax
open Mini_liquid.Constraints
open Mini_liquid.Rule_engine
open Mini_liquid.Solver

let int_type = OcamlType Predef.type_int

let int number =
  Constant { compiler_constant = Asttypes.Const_int number }

let apply name arguments = Apply (Variable name, arguments)

let empty_context = { variables = []; guards = [] }

let assert_equal label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "Assertion failed: %s" label)

let assert_true label value =
  if not value then failwith (Printf.sprintf "Assertion failed: %s" label)

let sum_binding =
  {
    name = "sum";
    recursive_status = true;
    definition_expression =
      Function
        ( "k",
          int_type,
          If
            ( apply "<" [ Variable "k"; int 0 ],
              int 0,
              Let
                ( {
                    name = "s";
                    recursive_status = false;
                    definition_expression =
                      apply "sum" [ apply "-" [ Variable "k"; int 1 ] ];
                  },
                  apply "+" [ Variable "s"; Variable "k" ] ) ) );
  }

let fibonacci_binding =
  {
    name = "fibonacci";
    recursive_status = true;
    definition_expression =
      Function
        ( "n",
          int_type,
          If
            ( apply "<" [ Variable "n"; int 1 ],
              int 0,
              If
                ( apply "<" [ Variable "n"; int 2 ],
                  int 1,
                  Let
                    ( {
                        name = "first";
                        recursive_status = false;
                        definition_expression =
                          apply "fibonacci" [ apply "-" [ Variable "n"; int 1 ] ];
                      },
                      Let
                        ( {
                            name = "second";
                            recursive_status = false;
                            definition_expression =
                              apply "fibonacci"
                                [ apply "-" [ Variable "n"; int 2 ] ];
                          },
                          apply "+" [ Variable "first"; Variable "second" ] ) ) ) ) );
  }

let solve binding =
  let template, obligations = infer_recursive_binding empty_context binding in
  let scopes = recursive_kappa_scopes empty_context binding in
  let initial, weakenings, final = solve_recursive_kappas scopes obligations in
  (template, obligations, initial, weakenings, final)

let () =
  let _, sum_obligations, sum_initial, _, sum_final = solve sum_binding in
  assert_equal "sum input Q*"
    [GreaterOrEqual (Name "result", Integer 0)]
    (List.assoc "sum_input" sum_initial);
  assert_equal "sum output Q*"
    [ GreaterOrEqual (Name "result", Name "k");
      FactGreaterThan (Name "k", Name "result");
      GreaterOrEqual (Name "result", Integer 0) ]
    (List.assoc "sum_output" sum_initial);
  assert_equal "sum input weakens to true" []
    (List.assoc "sum_input" sum_final);
  assert_equal "sum output inferred facts"
    [ GreaterOrEqual (Name "result", Name "k");
      GreaterOrEqual (Name "result", Integer 0) ]
    (List.assoc "sum_output" sum_final);
  assert_true "sum solved obligations"
    (List.for_all obligation_holds
       (List.map (fill_obligation_kappas sum_final) sum_obligations));

  let _, fibonacci_obligations, _, _, fibonacci_final = solve fibonacci_binding in
  assert_equal "fibonacci input is non-negative"
    [GreaterOrEqual (Name "result", Integer 0)]
    (List.assoc "fibonacci_input" fibonacci_final);
  assert_equal "fibonacci output is non-negative"
    [GreaterOrEqual (Name "result", Integer 0)]
    (List.assoc "fibonacci_output" fibonacci_final);
  assert_true "fibonacci solved obligations"
    (List.for_all obligation_holds
       (List.map (fill_obligation_kappas fibonacci_final) fibonacci_obligations));

  let substituted =
    apply_pending_substitutions
      [ { actual_param = Sub (Name "k", Integer 1); formal_param = "k" } ]
      [GreaterOrEqual (Name "result", Name "k")]
  in
  assert_equal "pending substitution"
    [GreaterOrEqual (Name "result", Sub (Name "k", Integer 1))]
    substituted;

  print_endline "mini-liquid tests passed"

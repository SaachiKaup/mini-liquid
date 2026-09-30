open Typedtree
open Syntax

exception Conversion_error of string

let fail message = raise (Conversion_error message)

let base_type_of_type type_expr =
  match Types.get_desc type_expr with
  | Tconstr (path, [], _) ->
      begin match Path.last path with
      | "int" -> IntType
      | "bool" -> BoolType
      | name -> fail ("Unsupported parameter type: " ^ name)
      end
  | _ -> fail "Unsupported parameter type"

let rec convert_expression expression =
  match expression.exp_desc with
  | Texp_ident (path, _, _) ->
      Variable (Path.last path)
  | Texp_constant (Const_int number) ->
      Constant (IntLiteral number)
  | Texp_ifthenelse (condition, then_branch, Some else_branch) ->
      If (
        convert_expression condition,
        convert_expression then_branch,
        convert_expression else_branch
      )
  | Texp_ifthenelse (_, _, None) ->
      fail "An if expression without an else branch is unsupported"
  | Texp_apply (function_expression, arguments) ->
      convert_application function_expression arguments
  | Texp_function (parameters, body) ->
      convert_function parameters body
  | _ ->
      fail "Unsupported Typedtree expression"

and convert_application function_expression arguments =
  let is_greater_than =
    match function_expression.exp_desc with
    | Texp_ident (path, _, _) ->
        Path.last path = ">"
    | _ -> false
  in
  if not is_greater_than then
    fail "Unsupported function application"
  else
    let supplied_arguments =
      List.filter_map
        (fun (_, argument) ->
          match argument with
          | Arg expression -> Some expression
          | Omitted () -> None)
        arguments
    in
    match supplied_arguments with
    | [left; right] ->
        GreaterThan (convert_expression left, convert_expression right)
    | _ ->
        fail "The greater-than operator must have two arguments"

and convert_function parameters body =
  let body_expression =
    match body with
    | Tfunction_body expression -> expression
    | Tfunction_cases _ ->
        fail "Pattern-matching function cases are unsupported"
  in
  let rec convert_parameters parameters expression =
    match parameters with
    | [] -> convert_expression expression
    | parameter :: remaining ->
        begin match parameter.fp_kind with
        | Tparam_pat pattern ->
            (* OCaml 5.5 stores the parameter's binding identifier directly
               in [fp_param]; the pattern is still used for its type. *)
            let name = Ident.name parameter.fp_param in
            let parameter_type = base_type_of_type pattern.pat_type in
            Function (
              name,
              parameter_type,
              convert_parameters remaining expression
            )
        | Tparam_optional_default _ ->
            fail "Optional function parameters are unsupported"
        end
  in
  convert_parameters parameters body_expression

let max_expression structure =
  let max_binding =
    List.find_map
      (fun item ->
        match item.str_desc with
        | Tstr_value (_, bindings) ->
            List.find_map
              (fun binding ->
                match binding.vb_pat.pat_desc with
                | Tpat_var (identifier, _, _)
                  when Ident.name identifier = "max" ->
                    Some binding.vb_expr
                | _ -> None)
              bindings
        | _ -> None)
      structure.str_items
  in
  match max_binding with
  | Some expression -> convert_expression expression
  | None -> fail "Could not find a top-level binding named max"

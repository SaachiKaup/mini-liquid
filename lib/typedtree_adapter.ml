open Typedtree
open Syntax

exception Conversion_error of string

let fail message = raise (Conversion_error message)

let base_type_of_type type_expr =
  OcamlType type_expr

let rec convert_expression expression =
  match expression.exp_desc with
  | Texp_ident (path, _, _) ->
      Variable (Path.last path)
  | Texp_constant compiler_constant ->
      Constant {
        compiler_constant;
      }
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
  | Texp_let (recursive_flag, bindings, body) ->
      begin match bindings with
      | [binding] ->
          Let (
            convert_binding recursive_flag binding,
            convert_expression body
          )
      | _ ->
          fail "Only a single local let binding is supported"
      end
  | _ ->
      fail "Unsupported Typedtree expression"

and convert_application function_expression arguments =
  let supplied_arguments =
    List.filter_map
      (fun (_, argument) ->
        match argument with
        | Arg expression -> Some (convert_expression expression)
        | Omitted () -> None)
      arguments
  in
  Apply (convert_expression function_expression, supplied_arguments)

and convert_binding recursive_flag binding =
  match binding.vb_pat.pat_desc with
  | Tpat_var (identifier, _, _) ->
      {
        name = Ident.name identifier;
        recursive_status = recursive_flag = Asttypes.Recursive;
        definition_expression = convert_expression binding.vb_expr;
      }
  | _ ->
      fail "Only variable bindings are supported"

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

let binding_named binding_name structure =
  let selected_binding =
    List.find_map
      (fun item ->
        match item.str_desc with
        | Tstr_value (recursive_flag, bindings) ->
            List.find_map
              (fun binding ->
                match binding.vb_pat.pat_desc with
                | Tpat_var (identifier, _, _)
                  when Ident.name identifier = binding_name ->
                    Some (convert_binding recursive_flag binding)
                | _ -> None)
              bindings
        | _ -> None)
      structure.str_items
  in
  match selected_binding with
  | Some binding -> binding
  | None ->
      fail (Printf.sprintf "Could not find a top-level binding named %s" binding_name)

let max_expression structure =
  (binding_named "max" structure).definition_expression

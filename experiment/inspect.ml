open Typedtree

let compiler_environment = ref None

let print_type_expr type_expr =
  match !compiler_environment with
  | Some environment ->
      Printtyp.wrap_printing_env ~error:false environment (fun () ->
        Printtyp.type_expr Format.std_formatter type_expr;
        Format.pp_print_flush Format.std_formatter ())
  | None ->
      Printtyp.type_expr Format.std_formatter type_expr;
      Format.pp_print_flush Format.std_formatter ()

let print_type (expression : expression) =
  Printf.printf " : ";
  print_type_expr expression.exp_type;
  print_newline ()

let find_max (structure : structure) =
  List.find_map
    (fun item ->
      match item.str_desc with
      | Tstr_value (_, bindings) ->
          List.find_map
            (fun binding ->
              match binding.vb_pat.pat_desc with
              | Tpat_var (id, _, _) when Ident.name id = "max" ->
                  Some binding.vb_expr
              | _ -> None)
            bindings
      | _ -> None)
    structure.str_items

let rec inspect expression =
  match expression.exp_desc with
  | Texp_function (parameters, body) ->
      Printf.printf "Texp_function";
      print_type expression;
      List.iter
        (fun parameter ->
          match parameter.fp_kind with
          | Tparam_pat pattern ->
              Printf.printf "  parameter %s" (Ident.name parameter.fp_param);
              print_type_expr pattern.pat_type;
              print_newline ()
          | Tparam_optional_default _ ->
              Printf.printf "  optional parameter %s\n"
                (Ident.name parameter.fp_param))
        parameters;
      (match body with
       | Tfunction_body body -> inspect body
       | Tfunction_cases _ -> print_endline "  function cases")
  | Texp_ifthenelse (condition, then_branch, Some else_branch) ->
      Printf.printf "Texp_ifthenelse";
      print_type expression;
      print_endline "  condition:";
      inspect condition;
      print_endline "  then:";
      inspect then_branch;
      print_endline "  else:";
      inspect else_branch
  | Texp_apply (function_expression, arguments) ->
      Printf.printf "Texp_apply";
      print_type expression;
      inspect function_expression;
      List.iter
        (fun (_, argument) ->
          match argument with
          | Arg argument -> inspect argument
          | Omitted () -> print_endline "  omitted argument")
        arguments
  | Texp_ident (path, _, _) ->
      Printf.printf "Texp_ident %s" (Path.name path);
      print_type expression
  | Texp_constant (Const_int number) ->
      Printf.printf "Texp_constant %d" number;
      print_type expression
  | _ ->
      Printf.printf "unsupported Typedtree node";
      print_type expression

let () =
  let cmt_path =
    if Array.length Sys.argv = 2 then Sys.argv.(1)
    else "max_program.cmt"
  in
  let cmt = Cmt_format.read_cmt cmt_path in
  compiler_environment := Some cmt.cmt_initial_env;
  match cmt.cmt_annots with
  | Implementation structure ->
      begin match find_max structure with
      | Some expression -> inspect expression
      | None -> failwith "Could not find a top-level binding named max"
      end
  | _ -> failwith "Expected implementation annotations in the .cmt file"

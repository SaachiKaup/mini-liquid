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

let string_of_rec_flag = function
  | Asttypes.Recursive -> "recursive"
  | Asttypes.Nonrecursive -> "nonrecursive"

let find_binding requested_name (structure : structure) =
  List.find_map
    (fun item ->
      match item.str_desc with
      | Tstr_value (rec_flag, bindings) ->
          List.find_map
            (fun binding ->
              match binding.vb_pat.pat_desc with
              | Tpat_var (id, _, _) ->
                  let name = Ident.name id in
                  begin match requested_name with
                  | None -> Some (name, rec_flag, binding.vb_expr)
                  | Some binding_name when name = binding_name ->
                      Some (name, rec_flag, binding.vb_expr)
                  | Some _ -> None
                  end
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
  | Texp_let (rec_flag, bindings, body) ->
      Printf.printf "Texp_let %s" (string_of_rec_flag rec_flag);
      print_type expression;
      List.iter
        (fun binding ->
          match binding.vb_pat.pat_desc with
          | Tpat_var (id, _, _) ->
              Printf.printf "  binding %s:\n" (Ident.name id)
          | _ ->
              print_endline "  non-variable binding:")
        bindings;
      List.iter (fun binding -> inspect binding.vb_expr) bindings;
      print_endline "  body:";
      inspect body
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
  let cmt_path, requested_name =
    match Array.length Sys.argv with
    | 1 -> ("max_program.cmt", None)
    | 2 -> (Sys.argv.(1), None)
    | 3 -> (Sys.argv.(1), Some Sys.argv.(2))
    | _ ->
        failwith "Usage: inspect [path-to-cmt] [top-level-binding-name]"
  in
  let cmt = Cmt_format.read_cmt cmt_path in
  compiler_environment := Some cmt.cmt_initial_env;
  match cmt.cmt_annots with
  | Implementation structure ->
      begin match find_binding requested_name structure with
      | Some (binding_name, rec_flag, expression) ->
          Printf.printf "Top-level binding %s is %s\n"
            binding_name (string_of_rec_flag rec_flag);
          inspect expression
      | None ->
          begin match requested_name with
          | Some binding_name ->
              failwith
                (Printf.sprintf "Could not find a top-level binding named %s"
                   binding_name)
          | None -> failwith "Could not find a named top-level binding"
          end
      end
  | _ -> failwith "Expected implementation annotations in the .cmt file"

open Typedtree
open Format
open Printtyp
open Types


(* Keep literals in the form supplied by the OCaml compiler, rather than
   reconstructing project-specific integer or Boolean representations. *)
type literal = {
  compiler_constant : Asttypes.constant;
}

type base_type =
  | OcamlType of Types.type_expr

type expr = 
  | Variable of string
  | Constant of literal
  | GreaterThan of expr * expr
  | If of expr * expr * expr
  | Function of string * base_type * expr

(* Top level binding type
*)
type binding = {
    name : string;
    recursive_status : bool;
    definition_expression : expr
}

type term =
  | Name of string
  | Integer of int

type fact =
  | Equal of term * term
  | FactGreaterThan of term * term
  | GreaterOrEqual of term * term
  | Not of fact

type kappa =
  | Kappa of string

type refinement_template =
  | KnownFacts of fact list
  | UnknownRefinement of kappa


(*
    Paper notation: {ν : B | κ}
    First line is for plain variables
    So the first line matches this - { result : int | κ0 }
    The second line is a translation of the LT-FUN rule
      (Γ; x : Tx ⊢ Q e : T) AND (Γ ⊢ x : Tx -> T)
      ─────────────────────────────────────────  [LT-FUN]
      Γ ⊢ Q λx.e : (x : Tx → T)
     > If, after assuming that x has type Tx, the function body e has type T, then the function fun x -> e has type “takes an x of type Tx and returns a T.”

*)
type liquid_type_template =
    | BaseLiquidType of base_type * refinement_template
    | FunctionLiquidType of string * liquid_type_template * liquid_type_template


let string_of_base_type = function
  | OcamlType type_expr ->
      Format.asprintf "%a" Printtyp.type_expr type_expr

let string_of_compiler_constant = function
  | Asttypes.Const_int number -> string_of_int number
  | Asttypes.Const_char character -> Printf.sprintf "%C" character
  | Asttypes.Const_string (value, _, None) -> Printf.sprintf "%S" value
  | Asttypes.Const_string (value, _, Some delimiter) ->
      Printf.sprintf "{%s|%s|%s}" delimiter value delimiter
  | Asttypes.Const_float value -> value
  | Asttypes.Const_int32 value -> Int32.to_string value ^ "l"
  | Asttypes.Const_int64 value -> Int64.to_string value ^ "L"
  | Asttypes.Const_nativeint value -> Nativeint.to_string value ^ "n"

let string_of_term = function
  | Name x -> x
  | Integer number -> string_of_int number

let rec string_of_fact = function
  | Equal (a_term, b_term) ->
      Printf.sprintf "%s = %s"
        (string_of_term a_term)
        (string_of_term b_term)
  | FactGreaterThan (a_term, b_term) ->
      Printf.sprintf "%s > %s"
        (string_of_term a_term)
        (string_of_term b_term)
  | GreaterOrEqual (a_term, b_term) ->
      Printf.sprintf "%s >= %s"
        (string_of_term a_term)
        (string_of_term b_term)
  | Not fact ->
      Printf.sprintf "not (%s)"
        (string_of_fact fact)

let rec string_of_expr = function
  | Variable name -> name
  | Constant literal -> string_of_compiler_constant literal.compiler_constant
  | GreaterThan (left, right) ->
      Printf.sprintf "(%s > %s)"
        (string_of_expr left)
        (string_of_expr right)
  | If (cond, then_block, else_block) ->
      Printf.sprintf "if %s then %s else %s"
        (string_of_expr cond)
        (string_of_expr then_block)
        (string_of_expr else_block)
  | Function (name, parameter_type, body) ->
      Printf.sprintf "fun %s : %s -> %s"
        name
        (string_of_base_type parameter_type)
        (string_of_expr body)

let string_of_kappa = function
  | Kappa name -> name

let string_of_refinement_template = function
  | KnownFacts [] -> "true"
  | KnownFacts facts ->
      String.concat " AND " (List.map string_of_fact facts)
  | UnknownRefinement kappa ->
      string_of_kappa kappa

let rec string_of_liquid_type_template = function
  | BaseLiquidType (base_type, refinement) ->
      Printf.sprintf "{result : %s | %s}"
        (string_of_base_type base_type)
        (string_of_refinement_template refinement)
  | FunctionLiquidType (name, input_type, output_type) ->
      Printf.sprintf "%s : %s -> %s"
        name
        (string_of_liquid_type_template input_type)
        (string_of_liquid_type_template output_type)

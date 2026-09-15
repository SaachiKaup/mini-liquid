type literal =
  | IntLiteral of int
  | BoolLiteral of bool

type base_type =
  | IntType
  | BoolType

type expr = 
  | Variable of string
  | Constant of literal
  | GreaterThan of expr * expr
  | If of expr * expr * expr
  | Function of string * base_type * expr

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
  | IntType -> "int"
  | BoolType -> "bool"

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
  | Constant (IntLiteral number) -> string_of_int number
  | Constant (BoolLiteral value) -> string_of_bool value
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


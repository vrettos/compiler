type unop = Positive | Negative | Fpositive | Fnegative | Not | Dereference
type binop =
    Add
  | Sub
  | Mul
  | Div
  | Fadd
  | Fsub
  | Fmul
  | Fdiv
  | Mod
  | Power
  | Naturaleq
  | Naturalneq
  | Less
  | Greater
  | Leq
  | Geq
  | Structeq
  | Structneq
  | And
  | Or
  | Semicolumn
  | Assign
type par = AST_par of string * Types.typ
type patt = { pa : pattern; ppos : Lexing.position * Lexing.position; }
and pattern =
    AST_pattern_int_const of int
  | AST_pattern_float_const of float
  | AST_pattern_char_const of char
  | AST_pattern_true
  | AST_pattern_false
  | AST_pattern_name of string
  | AST_pattern_Name of string
  | AST_pattern_constructor_call of string * patt list
type constr = AST_constr of string * Types.typ list
type tdef = AST_tdef of string * constr list
type typedef = AST_type of tdef list
type letdef = AST_let of bool * def list
and def = { de : definition; dpos : Lexing.position * Lexing.position; }
and definition =
    AST_def_normal of string * par list * Types.typ * expr
  | AST_def_mutable of string * expr list * Types.typ
and clause = AST_clause of patt * expr
and expr = { ex : expression; epos : Lexing.position * Lexing.position; }
and expression =
    AST_expr_unop of unop * expr
  | AST_expr_binop of expr * binop * expr
  | AST_expr_name of string
  | AST_expr_Name of string
  | AST_expr_letdef_in of letdef * expr
  | AST_expr_dim of int * string
  | AST_expr_if of expr * expr * expr
  | AST_expr_while of expr * expr
  | AST_expr_for of string * expr * bool * expr * expr
  | AST_expr_match of expr * clause list
  | AST_expr_int_const of int
  | AST_expr_float_const of float
  | AST_expr_char_const of char
  | AST_expr_string_const of string
  | AST_expr_true
  | AST_expr_false
  | AST_expr_unit
  | AST_expr_new of Types.typ
  | AST_expr_delete of expr
  | AST_expr_matrix_element of string * expr list
  | AST_expr_function_call of string * expr list
  | AST_expr_constructor_call of string * expr list
type def_tdef = AST_letdef of letdef | AST_typedef of typedef
type program = AST_program of def_tdef list

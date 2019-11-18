val typeinfer :
  Ast.program ->
  ((Types.typ * Types.typ) *
   ((Lexing.position * Lexing.position) * Types.typ * Types.typ))
  list -> Ast.program

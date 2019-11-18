type quad =
    Q_unit of Symbol.entry
  | Q_endu of Symbol.entry
  | Q_jump of int
  | Q_ifb of operand * int
  | Q_compare of operand * relation * operand * int
  | Q_calculate of operand * operator * operand * left
  | Q_assign of operand * left
  | Q_array of Symbol.entry * operand * left
  | Q_dim of Symbol.entry * int * left
  | Q_call of left * parameter list * result
  | Q_tailRecCall of Symbol.entry * parameter list
  | Q_return of operand
and operand = OP_constant of constant | OP_left of left
and constant =
    Const_int of int
  | Const_float of float
  | Const_bool of bool
  | Const_char of char
  | Const_string of string
and left = Left_entry of Symbol.entry | Left_deref of Symbol.entry
and parameter = Par of operand
and result = left option
and relation =
    EQ
  | NEQ
  | GE
  | LE
  | GEQ
  | LEQ
  | FEQ
  | FNEQ
  | FGE
  | FLE
  | FGEQ
  | FLEQ
  | CEQ
  | CNEQ
  | CGE
  | CLE
  | CGEQ
  | CLEQ
and operator = ADD | SUB | MUL | DIV | MOD | FADD | FSUB | FMUL | FDIV
and place =
    Place of operand
  | TFLists of quad ref list * quad ref list
  | Unit
  | TailRecCall
val getLeftTyp : left -> Types.typ
val getOPTyp : operand -> Types.typ
val equalLeft : left -> left -> bool
val star : int
val quad_dummy : quad
val nextQuad : Structs.IntMap.key ref
val genQuadRet : quad -> quad ref
val genQuad : quad -> unit
val patch : quad ref list -> int -> unit
val finalizeQuads : unit -> (Structs.IntMap.key * quad) list list

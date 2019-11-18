val errorMsg :
  Lexing.position * Lexing.position -> Types.typ -> Types.typ -> unit
val noPolyMsg : Lexing.position * Lexing.position -> Types.typ -> unit
val refToArrayMsg : Lexing.position * Lexing.position -> Types.typ -> unit
val arrayOfArraysMsg : Lexing.position * Lexing.position -> Types.typ -> unit
val funcRetArrayMsg : Lexing.position * Lexing.position -> Types.typ -> unit
val retFuncMsg :
  Lexing.position * Lexing.position -> Types.typ -> Types.typ -> unit
val noUdtTypes : unit -> unit
val dimMsg : Lexing.position * Lexing.position -> Types.typ -> int -> unit
val eqMsg : Lexing.position * Lexing.position -> Types.typ -> unit
val lessMsg : Lexing.position * Lexing.position -> Types.typ -> unit
val loopForeverMsg : Lexing.position * 'a -> string -> unit

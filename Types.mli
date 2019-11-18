type typ =
    TYPE_unit
  | TYPE_bool
  | TYPE_char
  | TYPE_int
  | TYPE_float
  | TYPE_array of typ * int
  | TYPE_ref of typ
  | TYPE_func of typ * typ
  | TYPE_id of string
  | TYPE_var of int
val sizeOfType : typ -> int
val typeVarNumber : int ref
val newTypeVar : unit -> typ
val pretty_typ : Format.formatter -> typ -> unit

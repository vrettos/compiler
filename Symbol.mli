type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_hidden : bool;
}
and variable_info = {
  variable_type : Types.typ;
  variable_allocated : bool;
  mutable variable_offset : int;
}
and function_info = {
  function_type : Types.typ;
  function_result_type : Types.typ;
  function_library : bool;
  mutable function_paramlist : entry list;
  mutable function_varlist : entry list;
  mutable function_templist : entry list;
  mutable function_vars_size : int;
  mutable function_pars_size : int;
  mutable function_body_label : string;
}
and parameter_info = {
  parameter_type : Types.typ;
  mutable parameter_offset : int;
}
and temporary_info = {
  temporary_type : Types.typ;
  mutable temporary_offset : int;
}
and type_info = { mutable type_constrslist : entry list; }
and constructor_info = {
  constructor_type : Types.typ;
  constructor_result_type : Types.typ;
}
and entry_info =
    ENTRY_variable of variable_info
  | ENTRY_function of function_info
  | ENTRY_parameter of parameter_info
  | ENTRY_temporary of temporary_info
  | ENTRY_type of type_info
  | ENTRY_constructor of constructor_info
and entry = {
  entry_id : Identifier.id;
  entry_name : string;
  entry_number : int;
  mutable entry_nesting : int;
  entry_scope : scope;
  entry_info : entry_info;
}
type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES
val currentScope : scope ref
val initSymbolTable : int -> unit
val openScope : unit -> unit
val closeScope : unit -> unit
val hideScope : scope -> bool -> unit
val start_positive_offset : int
val lookupEntry : string -> lookup_type -> bool -> entry
val newVariable : string -> Types.typ -> entry -> bool -> entry
val newAllocatedVariable : string -> Types.typ -> entry -> bool -> entry
val newParameter : string -> Types.typ -> entry -> bool -> entry
val newTemporary : Types.typ -> entry -> entry
val newFunctionInitStep : string -> Types.typ -> Types.typ -> bool -> entry
val newFunctionMidStep : entry -> unit
val newFunctionFixOffsets : entry -> unit
val newFunctionLastStep : entry -> unit
val newLibraryFunction : string -> Types.typ -> Types.typ -> bool -> entry
val newType : string -> bool -> entry
val newConstructor :
  string -> Types.typ -> Types.typ -> entry -> bool -> entry
val defined_type : Types.typ -> unit
val getEntryTyp : entry -> Types.typ
val getNameTyp : string -> Types.typ
val getEntryResTyp : entry -> Types.typ
val getNameResTyp : string -> Types.typ
val getEntryOffset : entry -> int
val newMakeArrayFunction : Types.typ -> entry
val newNewFunction : Types.typ -> entry
val newDeleteFunction : Types.typ -> entry

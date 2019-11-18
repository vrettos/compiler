open Identifier
open Error
open Types
open Structs


type scope = {
	sco_parent			: scope option;
	sco_nesting			: int;
	mutable sco_entries		: entry list;
	mutable sco_hidden		: bool
}
and variable_info = {
	variable_type			: Types.typ;
	variable_allocated		: bool;
	mutable variable_offset		: int
}
and function_info = {
	function_type			: Types.typ;
	function_result_type		: Types.typ;
	function_library		: bool;
	mutable function_paramlist	: entry list;
	mutable function_varlist	: entry list;
	mutable function_templist	: entry list;
	mutable function_vars_size	: int;
	mutable function_pars_size	: int;
	mutable function_body_label	: string
}
and parameter_info = {
	parameter_type			: Types.typ;
	mutable parameter_offset	: int
}
and temporary_info = {
	temporary_type			: Types.typ;
	mutable temporary_offset	: int
}
and type_info = {
	mutable type_constrslist	: entry list
}
and constructor_info = {
	constructor_type		: Types.typ;
	constructor_result_type		: Types.typ
}
and entry_info =
	| ENTRY_variable of variable_info
	| ENTRY_function of function_info
	| ENTRY_parameter of parameter_info
	| ENTRY_temporary of temporary_info
	| ENTRY_type of type_info
	| ENTRY_constructor of constructor_info
and entry = {
	entry_id			: Identifier.id;
	entry_name			: string;
	entry_number			: int;
	mutable entry_nesting		: int;
	entry_scope			: scope;
	entry_info			: entry_info
}


type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES


let prefix = "42"
let prefixReg = Str.regexp_string prefix
let clearId id =
	let name = id_name id in 
	if (Str.string_match prefixReg name 0) then
		id_make (Str.replace_first prefixReg "" name)
	else
		id_make name


let start_positive_offset = 8
let start_negative_offset = 0


let the_outer_scope = {
	sco_parent = None;
	sco_nesting = 0;
	sco_entries = [];
	sco_hidden = false
}


let currentScope = ref the_outer_scope
let nextTemp = ref 1
let nextNesting = ref (-1)
let nextNumber = ref 1


let tab = ref (H.create 0)


let initSymbolTable size =
	tab := H.create size;
	currentScope := the_outer_scope;
	nextTemp := 1;
	nextNesting := -1;
	nextNumber := 1


let openScope () =
	let sco = {
		sco_parent = Some !currentScope;
		sco_nesting = !currentScope.sco_nesting + 1;
		sco_entries = [];
		sco_hidden = false
	}
	in
	currentScope := sco


let closeScope () =
	let sco = !currentScope in
	let manyentry e = H.remove !tab e.entry_id in
	List.iter manyentry sco.sco_entries;
	match sco.sco_parent with
	| Some scp ->
		currentScope := scp
	| None ->
		internal "cannot close the outer scope!"


let hideScope sco flag = sco.sco_hidden <- flag


exception Failure_NewEntry of entry


let newEntry name inf err =
	let id = Identifier.id_make name in
	try
		if err then begin
			try
				let e = H.find !tab id in
				if e.entry_scope.sco_nesting = !currentScope.sco_nesting then
					raise (Failure_NewEntry e)
			with Not_found ->
				()
		end;
		let e = {
			entry_id = id;
			entry_name = name;
			entry_number = !nextNumber;
			entry_nesting = !nextNesting;
			entry_scope = !currentScope;
			entry_info = inf
		}
		in
		incr nextNumber;
		H.add !tab id e;
		!currentScope.sco_entries <- e :: !currentScope.sco_entries;
		e
	with Failure_NewEntry e ->
		error "duplicate identifier %a" pretty_id (clearId id);
		e


let lookupEntry name how err =
	let id = Identifier.id_make name in
	let scc = !currentScope in
	let lookup () =
		match how with
		| LOOKUP_CURRENT_SCOPE ->
			let e = H.find !tab id in
			if e.entry_scope.sco_nesting = scc.sco_nesting then
				e
			else
				raise Not_found
		| LOOKUP_ALL_SCOPES ->
			let rec walk es =
				match es with
				| [] ->
					raise Not_found
				| e :: es ->
					if not e.entry_scope.sco_hidden then
						e
					else
						walk es 
			in
			walk (H.find_all !tab id) 
	in
	if err then
		try
			lookup ()
		with Not_found ->
			error "unknown identifier %a (first occurrence)" pretty_id (clearId id);
			raise Exit
	else
		lookup ()


let newVariableAux name typ f alloc err =
	match f.entry_info with
	| ENTRY_function inf ->
		let inf_v = {
			variable_type = typ;
			variable_allocated = alloc;
			variable_offset = 0
		}
		in
		let e = newEntry name (ENTRY_variable inf_v) err in
		inf.function_varlist <- e :: inf.function_varlist;
		e
	| _ ->
		internal "Cannot add a variable to a non-function";
		raise Exit


let newVariable name typ f err = newVariableAux name typ f false err


let newAllocatedVariable name typ f err = newVariableAux name typ f true err


let newParameter name typ f err =
	match f.entry_info with
	| ENTRY_function inf ->
		let inf_p = {
			parameter_type = typ;
			parameter_offset = 0
		} 
		in
		let e = newEntry name (ENTRY_parameter inf_p) err in
		inf.function_paramlist <- e :: inf.function_paramlist;
		e
	| _ ->
		internal "Cannot add a parameter to a non-function";
		raise Exit


let newTemporary typ f =
	match f.entry_info with
	| ENTRY_function inf ->
		let name = "$" ^ (string_of_int !nextTemp) in
		let inf_t = {
			temporary_type = typ;
			temporary_offset = 0
		} 
		in
		incr nextTemp;
		let e = newEntry name (ENTRY_temporary inf_t) false in
		inf.function_templist <- e :: inf.function_templist;
		e
	| _ ->
		internal "Cannot add a temporary to a non-function";
		raise Exit


let newFunctionInitStepAux name typ resTyp lib err =
	let id = Identifier.id_make name in
	try
		ignore (lookupEntry name LOOKUP_CURRENT_SCOPE false);
		if err then
			error "duplicate identifier: %a" pretty_id (clearId id);
			raise Exit
	with Not_found ->
	let inf = {
		function_paramlist = [];
		function_varlist = [];
		function_library = lib;
		function_templist = [];
		function_type = typ;
		function_result_type = resTyp;
		function_vars_size = 0;
		function_pars_size = 0;
		function_body_label = ""
	} 
	in
	newEntry name (ENTRY_function inf) false


let newFunctionInitStep name typ resTyp err =
	newFunctionInitStepAux name typ resTyp false err


let newFunctionMidStep e =
	incr nextNesting;
	e.entry_nesting <- !nextNesting


let newFunctionFixOffsets e =
	match e.entry_info with
	| ENTRY_function inf -> begin
		let offset = ref start_positive_offset in
		let fix_offset e =
			match e.entry_info with
			| ENTRY_parameter inf_p ->
				inf_p.parameter_offset <- !offset;
				let size = sizeOfType inf_p.parameter_type in
				offset := !offset + size;
				inf.function_pars_size <- inf.function_pars_size + size
			| ENTRY_variable inf_v ->
				let size = sizeOfType inf_v.variable_type in
				offset := !offset - size;
				inf.function_vars_size <- inf.function_vars_size + size;
				inf_v.variable_offset <- !offset
			| ENTRY_temporary inf_t ->
				let size = sizeOfType inf_t.temporary_type in
				offset := !offset - size;
				inf.function_vars_size <- inf.function_vars_size + size;
				inf_t.temporary_offset <- !offset
			| _ ->
				internal "Cannot fix offset to function, constructor, type" 
		in
		inf.function_vars_size <- 0;
		inf.function_pars_size <- 0;
		offset := start_positive_offset;
		List.iter fix_offset inf.function_paramlist;
		offset := start_negative_offset;
		List.iter fix_offset inf.function_varlist;
		List.iter fix_offset inf.function_templist
	  end
	| _ ->
		internal "Cannot fix offsets in a non-function"


let newFunctionLastStep e =
	newFunctionFixOffsets e;
	decr nextNesting


let newLibraryFunction id typ resTyp err =
	let e = newFunctionInitStepAux id typ resTyp true err in
	newFunctionMidStep e;
	newFunctionLastStep e;
	e


let newType name err =
	let name = prefix ^ name in
	let id = Identifier.id_make name in
	try
		ignore (lookupEntry name LOOKUP_CURRENT_SCOPE false);
		if err then
			error "duplicate type definition: %a" pretty_id (clearId id);
			raise Exit
	with Not_found ->
	let inf = {
		type_constrslist = []
	}
	in
	newEntry name (ENTRY_type inf) false


let newConstructor name typ resTyp f err =
	match f.entry_info with
	| ENTRY_type inf ->
		let inf_c = {
			constructor_type = typ;
			constructor_result_type = resTyp
		} 
		in
		let e = newEntry name (ENTRY_constructor inf_c) err in
		inf.type_constrslist <- e :: inf.type_constrslist;
		e
	| _ ->
		internal "Cannot add a constructor to a non-type";
		raise Exit


let defined_type typ =
	match typ with
	| TYPE_id name -> begin
		try ignore (lookupEntry (prefix ^ name) LOOKUP_ALL_SCOPES false) with
		| Not_found ->
			fatal "Undefined type %a" pretty_typ typ
	  end
	| _ ->
		()


let getEntryTyp e = 
	match e.entry_info with
        | ENTRY_variable inf ->
		inf.variable_type
        | ENTRY_function inf ->
		inf.function_type
        | ENTRY_parameter inf ->
		inf.parameter_type
        | ENTRY_temporary inf ->
		inf.temporary_type
	| ENTRY_constructor inf ->
		inf.constructor_type
	| ENTRY_type _ ->
		internal "Types have no type";
		raise Exit


let getNameTyp name =
        getEntryTyp (lookupEntry name LOOKUP_ALL_SCOPES true)


let getEntryResTyp e =
	let rec getResTyp typ =
		match typ with
		| TYPE_func (_, resTyp) ->
			getResTyp resTyp
		| _ ->
			typ
	in
	match e.entry_info with
	| ENTRY_function inf ->
		inf.function_result_type
	| ENTRY_constructor inf ->
		inf.constructor_result_type
	| ENTRY_variable inf ->
		getResTyp inf.variable_type
	| ENTRY_parameter inf ->
		getResTyp inf.parameter_type
	| ENTRY_temporary inf ->
		getResTyp inf.temporary_type
	| ENTRY_type _ ->
		internal "Types have no result type";
		raise Exit


let getNameResTyp name =
	getEntryResTyp (lookupEntry name LOOKUP_ALL_SCOPES true)


let getEntryOffset e =
	match e.entry_info with
	| ENTRY_variable inf ->
		inf.variable_offset
	| ENTRY_parameter inf ->
		inf.parameter_offset
	| ENTRY_temporary inf ->
		inf.temporary_offset
	| ENTRY_function _
	| ENTRY_type _
	| ENTRY_constructor _ ->
		internal "Functions, types, constructors have no offset";
		raise Exit


let newMakeArrayFunction typ =
	(* vazw auton ton tupo logo tou hack stin assembly *)
	let entry_make_array_typ = TYPE_func (TYPE_int, TYPE_func (TYPE_int, typ)) in
	openScope ();
	let entry_make_array = newLibraryFunction "_make_array" entry_make_array_typ typ true in
	closeScope ();
	entry_make_array

let newNewFunction typ =
	let entry_new_typ = TYPE_func (TYPE_int, typ) in
	openScope ();
	let entry_new = newLibraryFunction "_new" entry_new_typ typ true in
	closeScope ();
	entry_new


let newDeleteFunction typ =
	let entry_delete_typ = TYPE_func (typ, TYPE_unit) in
	openScope ();
	let entry_delete = newLibraryFunction "_dispose" entry_delete_typ TYPE_unit true in
	closeScope ();
	entry_delete



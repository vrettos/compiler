open Ast
open Error
open Types
open ErrorMsgs
open Symbol
open LibraryFunctions


let refToArray pos initTyp =
	let rec refToArrayAux typ =
		match typ with
		| TYPE_ref (TYPE_array (_, _)) ->
			ErrorMsgs.refToArrayMsg pos initTyp
		| TYPE_ref tau
		| TYPE_array (tau, _) ->
			refToArrayAux tau
		| TYPE_func (tau1, tau2) ->
			refToArrayAux tau1;
			refToArrayAux tau2
		| _ ->
			()
	in
	refToArrayAux initTyp


let arrayOfArrays pos initTyp =
	let rec arrayOfArraysAux typ =
		match typ with
		| TYPE_array (TYPE_array (_, _), _) ->
			ErrorMsgs.arrayOfArraysMsg pos initTyp
		| TYPE_ref tau
		| TYPE_array (tau, _) ->
			arrayOfArraysAux tau
		| TYPE_func (tau1, tau2) ->
			arrayOfArraysAux tau1;
			arrayOfArraysAux tau2
		| _ ->
			()
	in
	arrayOfArraysAux initTyp


let funcRetArray pos initTyp =
	let rec funcRetArrayAux typ =
		match typ with
		| TYPE_func (_, TYPE_array (_, _)) ->
			ErrorMsgs.funcRetArrayMsg pos initTyp
		| TYPE_func (tau1, tau2) ->
			funcRetArrayAux tau1;
			funcRetArrayAux tau2
		| TYPE_ref tau
		| TYPE_array (tau, _) ->
			funcRetArrayAux tau
		| _ ->
			()
	in
	funcRetArrayAux initTyp



let retFunc pos typ resTyp =
	match resTyp with
	| TYPE_func _ ->
		ErrorMsgs.retFuncMsg pos typ resTyp
	| _ ->
		()


let rec check (AST_program dl) =
	initSymbolTable 256;
	ignore (addLibrary ());
	let outer_func = newFunctionInitStep "_outer" (TYPE_func(TYPE_unit, TYPE_unit)) TYPE_unit true in
	newFunctionMidStep outer_func;
	List.iter (def_tdef_check outer_func) dl;
	newFunctionLastStep outer_func;
	List.iter (fun _ -> closeScope ()) dl


and def_tdef_check outer_func d =
	match d with
	| AST_letdef letdef ->
		letdef_check outer_func letdef
	| AST_typedef _ ->
		openScope ();
		ErrorMsgs.noUdtTypes ()


and letdef_check outer_func (AST_let (recursive, defl)) = 
	let getFunctionType parl typ =
		let process (AST_par (_, argTyp)) resTyp =
			TYPE_func (argTyp, resTyp)
		in
		List.fold_right process parl typ
	in
	let def_init_step def =
		match def.de with
		| AST_def_normal (name, parl, typ, _) ->
			if (parl <> []) then
				newFunctionInitStep name (getFunctionType parl typ) typ true
			else
				newVariable name typ outer_func true
		| AST_def_mutable (name, exprl, typ) ->
			if (exprl <> []) then
				newAllocatedVariable name (TYPE_array (typ, List.length exprl)) outer_func true
			else
				newVariable name (TYPE_ref typ) outer_func true
	in
	openScope ();
	hideScope !currentScope (not recursive);
	let entries = List.map def_init_step defl in
	let defXentryList = List.combine defl entries in
	List.iter (def_check outer_func) defXentryList;
	hideScope !currentScope false


and def_check outer_func (def, ent) =
	let add_pars func_ent parl =
		let add_one_par (AST_par (name, typ)) =
			(* err = false, dioti borei na exoume episkiasi parametrwn *)
			ignore (newParameter name typ func_ent false)
		in
		List.iter add_one_par parl
	in
	match def.de with
	(* to entry name den einai sto symbol table an recursive = false *)
	| AST_def_normal (_, parl, typ, expr) ->
		let tau = getEntryTyp ent in
		refToArray def.dpos tau;
		arrayOfArrays def.dpos tau;
		funcRetArray def.dpos tau;
		if (parl <> []) then (
			retFunc def.dpos tau typ;
			newFunctionMidStep ent;
			openScope ();
			add_pars ent parl;
			ignore (expr_check ent expr);
			newFunctionLastStep ent;
			closeScope ()
		)
		else (
			ignore (expr_check outer_func expr)
		)
	| AST_def_mutable (_, exprl, _) ->
		let tau = getEntryTyp ent in
		refToArray def.dpos tau;
		arrayOfArrays def.dpos tau;
		funcRetArray def.dpos tau;
		List.iter (fun e -> ignore (expr_check outer_func e)) exprl


and expr_check outer_func expr =
	match expr.ex with
	| AST_expr_binop (expr1, bn, expr2) ->
		binop_check outer_func (expr1, bn, expr2)
	| AST_expr_unop (un, expr) ->
		unop_check outer_func (un, expr)
	| AST_expr_dim (d, name) -> begin
		let tau = getNameTyp name in
		match tau with
		| TYPE_array (_, dim) ->
			if ((dim < d) || (d < 1)) then (
				ErrorMsgs.dimMsg expr.epos tau d
			);
			TYPE_int
		| _ ->
			internal "It should be of type array because of the constraint";
			raise Exit
	  end
        | AST_expr_matrix_element (name, exprl) -> begin
		List.iter (fun e -> ignore (expr_check outer_func e)) exprl;
		let tau = getNameTyp name in
		match tau with
		| TYPE_array (typ, _) ->
			TYPE_ref typ
		| _ ->
			internal "It should be of type array because of the constraint";
			raise Exit
	  end
	| AST_expr_if (expr1, expr2, expr3) -> 
		ignore (expr_check outer_func expr1);
		ignore (expr_check outer_func expr2);
		expr_check outer_func expr3
	| AST_expr_while (expr1, expr2) ->
		ignore (expr_check outer_func expr1);
		ignore (expr_check outer_func expr2);
		TYPE_unit
	| AST_expr_for (name, expr1, _, expr2, expr3) ->
		ignore (expr_check outer_func expr1);
		ignore (expr_check outer_func expr2);
		openScope ();
		ignore (newVariable name TYPE_int outer_func false);
		ignore (expr_check outer_func expr3);
		closeScope ();
		TYPE_unit
	| AST_expr_new typ ->
		let tau = TYPE_ref typ in
		refToArray expr.epos tau;
		arrayOfArrays expr.epos tau;
		funcRetArray expr.epos tau;
		tau
	| AST_expr_delete expr ->
		ignore (expr_check outer_func expr);
		TYPE_unit
	| AST_expr_letdef_in (letdef, expr) ->
		letdef_check outer_func letdef;
		let tau = expr_check outer_func expr in
		closeScope ();
		tau
	| AST_expr_function_call (name, exprl) ->
		let tau = getNameTyp name in
		let rec check_partial_application typ args =
			match typ with
			| TYPE_func (_, resTyp) when (0 < args) ->
				check_partial_application resTyp (args - 1)
			| TYPE_func (_, _) when (0 = args) ->
				ErrorMsgs.retFuncMsg expr.epos tau typ;
				typ
			| _ when (0 = args) ->
				typ
			| _ ->
				internal "Function was expecting less arguments";
				raise Exit
		in
		check_partial_application tau (List.length exprl)
	| AST_expr_name name ->
		getNameTyp name
	| AST_expr_int_const _ ->
		TYPE_int
	| AST_expr_float_const _ ->
		TYPE_float
        | AST_expr_char_const _ ->
		TYPE_char
        | AST_expr_string_const _ ->
		TYPE_array (TYPE_char, 1)
        | AST_expr_true
        | AST_expr_false ->
		TYPE_bool
        | AST_expr_unit ->
		TYPE_unit
	| AST_expr_Name _
	| AST_expr_constructor_call _
	| AST_expr_match _ ->
		ErrorMsgs.noUdtTypes ();
		TYPE_id "none"


and binop_check outer_func (expr1, bn, expr2) =
	ignore (expr_check outer_func expr1);
        let tau2 = expr_check outer_func expr2 in
	let eqCheck pos typ =
		match typ with
		| TYPE_func _
		| TYPE_array _  ->
			ErrorMsgs.eqMsg pos typ
		| _ ->
			()
	in
	let lessCheck pos typ =
		match typ with
		| TYPE_int
		| TYPE_char
		| TYPE_float ->
			()
		| _ ->
			ErrorMsgs.lessMsg pos typ
	in
	match bn with
	| Add
        | Sub
        | Mul
        | Div
	| Mod
	| Fadd
        | Fsub
        | Fmul
        | Fdiv
	| Power
	| And
	| Or 
	| Semicolumn ->
		tau2
	| Naturaleq
        | Naturalneq
	| Structeq
	| Structneq ->
		eqCheck expr2.epos tau2;
		TYPE_bool
	| Less
        | Greater
        | Leq
        | Geq ->
		lessCheck expr2.epos tau2;
		TYPE_bool
        | Assign ->
		TYPE_unit


and unop_check outer_func (un, expr) =
	let tau = expr_check outer_func expr in
	match un with
	| Dereference -> begin
		match tau with
		| TYPE_ref typ ->
			typ
		| _ ->
			internal "It should be of type ref because of the constraint";
			tau
	  end
	| Not ->
		TYPE_bool
	| Positive
	| Negative ->
		TYPE_int
	| Fpositive
	| Fnegative ->
		TYPE_float



open Types
open Symbol
open Ast
open LibraryFunctions
open ErrorMsgs


let rec generateConstraints (AST_program dl) =
	initSymbolTable 256;
	ignore (addLibrary ());
	let outer_func = newFunctionInitStep "_outer" (TYPE_func(TYPE_unit, TYPE_unit)) TYPE_unit true in
	newFunctionMidStep outer_func;
	let c = List.concat (List.map (def_tdef_gen outer_func) dl) in
	newFunctionLastStep outer_func;
	List.iter (fun _ -> closeScope ()) dl;
	c


and def_tdef_gen outer_func d =
	match d with
	| AST_letdef letdef ->
		letdef_gen outer_func letdef
	| AST_typedef typedef ->
		typedef_gen typedef


and typedef_gen (AST_type tdefl) =
	let tdef_init_step (AST_tdef (name, _)) =
		newType name true
	in
	let getConstructorType (AST_constr (_, typl)) typ =
		List.fold_right 
			(fun argTyp resTyp -> defined_type argTyp; TYPE_func (argTyp, resTyp)) 
			typl 
			typ
	in
	let tdef_add (AST_tdef (type_name, constrl), type_ent) =
		let constr_add (AST_constr (constr_name, typl)) =
			let typ = getConstructorType (AST_constr (constr_name, typl)) (TYPE_id type_name) in
			ignore (newConstructor constr_name typ (TYPE_id type_name) type_ent true)
		in
		List.iter constr_add constrl
	in
	openScope ();
	let entries = List.map tdef_init_step tdefl in
	let tdefXentryList = List.combine tdefl entries in
	List.iter tdef_add tdefXentryList;
	[]


and letdef_gen outer_func (AST_let (recursive, defl)) =
	let getFunctionType parl typ =
		let process (AST_par (_, argTyp)) resTyp =
			defined_type argTyp;
			TYPE_func (argTyp, resTyp)
		in
		List.fold_right process parl typ
	in
	let def_init_step def =
		match def.de with
		| AST_def_normal (name, parl, typ, _) ->
			defined_type typ;
			if (parl <> []) then
				newFunctionInitStep name (getFunctionType parl typ) typ true
			else
				newVariable name typ outer_func true
		| AST_def_mutable (name, exprl, typ) ->
			defined_type typ;
			if (exprl <> []) then
				newAllocatedVariable name (TYPE_array (typ, List.length exprl)) outer_func true
			else
				newVariable name (TYPE_ref typ) outer_func true
	in
	openScope ();
	hideScope !currentScope (not recursive);
	let entries = List.map def_init_step defl in
	let defXentryList = List.combine defl entries in
	let c = List.concat (List.map (def_gen outer_func) defXentryList) in
	hideScope !currentScope false;
	c


and def_gen outer_func (def, ent) =
	let add_pars func_ent parl =
		let add_one_par (AST_par (name, typ)) =
			(* err = false, dioti borei na exoume episkiasi parametrwn *)
			ignore (newParameter name typ func_ent false)
		in
		List.iter add_one_par parl
	in
	match def.de with
	| AST_def_normal (name, parl, typ, expr) ->
		if (parl <> []) then (
			newFunctionMidStep ent;
			openScope ();
			add_pars ent parl;
			let (tau, c) = expr_gen ent expr in
			newFunctionLastStep ent;
			closeScope ();
			((typ, tau), (def.dpos, typ, tau)) :: c
		)
		else (
			let (tau, c) = expr_gen outer_func expr in
			((typ, tau), (def.dpos, typ, tau)) :: c
		)
	| AST_def_mutable (_, exprl, typ) ->
		if (exprl <> []) then (
			dimExprl_gen outer_func exprl
		)
		else (
			[]
		)


and dimExprl_gen outer_func exprl =
	let gen_one_dim acc expr =
		let (tau, c) = expr_gen outer_func expr in	
		((tau, TYPE_int), (expr.epos, tau, TYPE_int)) :: (c @ acc)
	in
	List.fold_left gen_one_dim [] exprl


and expr_gen outer_func expr =
	match expr.ex with
	| AST_expr_name name ->
		(getNameTyp name, [])
	| AST_expr_Name name ->
		(getNameTyp name, [])
	| AST_expr_int_const _ ->
		(TYPE_int, [])
	| AST_expr_float_const _ ->
		(TYPE_float, [])
        | AST_expr_char_const _ ->
		(TYPE_char, [])
        | AST_expr_string_const _ ->
		(TYPE_array (TYPE_char, 1), [])
        | AST_expr_true
        | AST_expr_false ->
		(TYPE_bool, [])
        | AST_expr_unit ->
		(TYPE_unit, [])
	| AST_expr_binop (expr1, bn, expr2) ->
		binop_gen outer_func (expr1, bn, expr2)
	| AST_expr_unop (un, expr) ->
		unop_gen outer_func (un, expr)
	| AST_expr_dim (_, _) ->
		(TYPE_int, [])
        | AST_expr_matrix_element (name, exprl) ->
		let c = dimExprl_gen outer_func exprl in
		let tau1 = getNameTyp name in
		let typ = newTypeVar () in
		let tau2 = TYPE_array (typ, List.length exprl) in
		(TYPE_ref typ, ((tau1, tau2), (expr.epos, tau1, tau2)) 
		:: c)
	| AST_expr_if (expr1, expr2, expr3) -> 
		let (tau1, c1) = expr_gen outer_func expr1 in
		let (tau2, c2) = expr_gen outer_func expr2 in
		let (tau3, c3) = expr_gen outer_func expr3 in
		(tau2, ((tau1, TYPE_bool), (expr1.epos, tau1, TYPE_bool))
		:: ((tau3, tau2), (expr3.epos, tau3, tau2)) :: (c1 @ c2 @ c3))
	| AST_expr_while (expr1, expr2) ->
		let (tau1, c1) = expr_gen outer_func expr1 in
		let (tau2, c2) = expr_gen outer_func expr2 in
		(TYPE_unit, ((tau1, TYPE_bool), (expr1.epos, tau1, TYPE_bool))
		:: ((tau2, TYPE_unit), (expr2.epos, tau2, TYPE_unit)) :: (c1 @ c2))
	| AST_expr_for (name, expr1, _, expr2, expr3) ->
		let (tau1, c1) = expr_gen outer_func expr1 in
		let (tau2, c2) = expr_gen outer_func expr2 in
		openScope ();
		ignore (newVariable name TYPE_int outer_func false);
		let (tau3, c3) = expr_gen outer_func expr3 in
		closeScope ();
		(TYPE_unit, ((tau1, TYPE_int), (expr1.epos, tau1, TYPE_int))
		:: ((tau2, TYPE_int), (expr2.epos, tau2, TYPE_int))
		:: ((tau3, TYPE_unit), (expr3.epos, tau3, TYPE_unit))
		:: (c1 @ c2 @ c3))
	| AST_expr_new typ ->
		defined_type typ;
		(TYPE_ref typ, [])
	| AST_expr_delete expr ->
		let (tau, c) = expr_gen outer_func expr in
		let typ = newTypeVar () in
		(TYPE_unit, ((tau, TYPE_ref typ), (expr.epos, tau, TYPE_ref typ)) :: c)
	| AST_expr_letdef_in (letdef, expr) ->
		let c1 = letdef_gen outer_func letdef in
		let (tau, c2) = expr_gen outer_func expr in
		closeScope ();
		(tau, c1 @ c2)
	| AST_expr_function_call (name, exprl)
	| AST_expr_constructor_call (name, exprl) ->
		let typ = getNameTyp name in
		(* den prepei na xrisimopoiisoume getNameResTyp,
		   giati borei o tupos na einai @1 *)
		let resTyp = newTypeVar () in
		let (tau, c) = realPars_gen outer_func exprl resTyp in
		(resTyp, ((typ, tau), (expr.epos, typ, tau)) :: c)
	| AST_expr_match (initExpr, clausel) ->
		let (tau1, c1) = expr_gen  outer_func initExpr in
		let (_, resTyp, c) = clauselist_gen  outer_func clausel tau1 in
		(resTyp, c1 @ c)


and clauselist_gen outer_func clausel initPatTyp =
	let process (patTyp1, exprTyp1, c1) clause =
		let (patTyp2, exprTyp2, c2) = clause_gen outer_func clause in
		let AST_clause (pat, expr) = clause in
		let c3 = ((patTyp2, patTyp1), (pat.ppos, patTyp2, patTyp1))
			:: ((exprTyp2, exprTyp1), (expr.epos, exprTyp2, exprTyp1))
			:: (c2 @ c1)
		in
		(patTyp2, exprTyp2, c3)
	in
	let initExprTyp = newTypeVar ()  in
	List.fold_left process (initPatTyp, initExprTyp, []) clausel


and clause_gen outer_func (AST_clause (pat, expr)) =
	openScope ();
	let (patTyp, c1) = pattern_gen outer_func pat in
	let (exprTyp, c2) = expr_gen outer_func expr in
	closeScope ();
	(patTyp, exprTyp, c1 @ c2)


and pattern_gen outer_func pat =
	match pat.pa with
	| AST_pattern_int_const _ ->
		(TYPE_int, [])
	| AST_pattern_float_const _ ->
		(TYPE_float, [])
	| AST_pattern_char_const _ ->
		(TYPE_char, [])
	| AST_pattern_true ->
		(TYPE_bool, [])
	| AST_pattern_false ->
		(TYPE_bool, [])
	| AST_pattern_name name ->
		let typ = newTypeVar () in
		ignore (newVariable name typ outer_func false);
		(typ, [])
	| AST_pattern_Name name ->
		let resTyp = getNameResTyp name in
		(resTyp, [])
	| AST_pattern_constructor_call (name, patternl) ->
		let typ = getNameTyp name in
		let resTyp = getNameResTyp name in
		let (tl, cl) = List.split (List.map (pattern_gen outer_func) patternl) in
		let tau = List.fold_right (fun x y -> (TYPE_func(x, y))) tl resTyp in
		(resTyp, ((typ, tau), (pat.ppos, typ, tau)) :: (List.concat cl))


and realPars_gen outer_func exprl finalResTyp =
	let one_par_gen expr (resTyp, c1) =
		let (argTyp, c2) = expr_gen outer_func expr in
		(TYPE_func (argTyp, resTyp), c1 @ c2)
	in
	List.fold_right one_par_gen exprl (finalResTyp, [])


and binop_gen outer_func (expr1, bn, expr2) =
	let (tau1, c1) = expr_gen outer_func expr1 in
        let (tau2, c2) = expr_gen outer_func expr2 in
	let c = c1 @ c2 in
	match bn with
	| Add
        | Sub
        | Mul
        | Div
	| Mod ->
        	(TYPE_int, ((tau1, TYPE_int), (expr1.epos, tau1, TYPE_int))
		:: ((tau2, TYPE_int), (expr2.epos, tau2, TYPE_int)) :: c)
	| Fadd
        | Fsub
        | Fmul
        | Fdiv
	| Power ->
		(TYPE_float, ((tau1, TYPE_float), (expr1.epos, tau1, TYPE_float))
		:: ((tau2, TYPE_float), (expr2.epos, tau2, TYPE_float)) :: c)
	| Naturaleq
        | Naturalneq
	| Less
        | Greater
        | Leq
        | Geq
	| Structeq
        | Structneq ->
		(TYPE_bool, ((tau1, tau2), (expr2.epos, tau2, tau1)) :: c)
	| And
        | Or ->
		(TYPE_bool, ((tau1, TYPE_bool), (expr1.epos, tau1, TYPE_bool))
		:: ((tau2, TYPE_bool), (expr2.epos, tau2, TYPE_bool)) :: c)
	| Semicolumn ->
	 	(tau2, c)
        | Assign ->
		let typ = newTypeVar () in
		(TYPE_unit, ((tau2, typ), (expr2.epos, tau2, typ)) :: 
		((tau1, TYPE_ref typ), (expr1.epos, tau1, TYPE_ref typ)) :: c)


and unop_gen outer_func (un, expr) =		
	let (tau, c) = expr_gen outer_func expr in
	match un with
	| Positive
	| Negative ->
		(TYPE_int, ((tau, TYPE_int), (expr.epos, tau, TYPE_int)) :: c)
	| Fpositive
	| Fnegative ->
		(TYPE_float, ((tau, TYPE_float), (expr.epos, tau, TYPE_float)) :: c)
	| Not ->
		(TYPE_bool, ((tau, TYPE_bool), (expr.epos, tau, TYPE_bool)) :: c)
	| Dereference ->
		let typ = newTypeVar () in
		(typ, ((tau, TYPE_ref typ), (expr.epos, tau, (TYPE_ref typ))) :: c)















open Types
open Structs
open ErrorMsgs
open Ast


let rec sub map tau =
	match tau with
	| TYPE_var n -> begin
		try (
			let newTau = IntMap.find n map in
			sub map newTau
		) with Not_found ->
			tau
	  end
	| TYPE_func (tau1, tau2) ->
		TYPE_func (sub map tau1, sub map tau2)
	| TYPE_array (tau1, dim) ->
		TYPE_array (sub map tau1, dim)
	| TYPE_ref tau1 ->
		TYPE_ref (sub map tau1)
	| TYPE_int
	| TYPE_unit
	| TYPE_bool
	| TYPE_char
	| TYPE_float
	| TYPE_id _ ->
		tau


let subc alpha tau c =
	let map = IntMap.add alpha tau IntMap.empty in
	let walk ((tau1, tau2), msg) = ((sub map tau1, sub map tau2), msg) in
	List.map walk c


let rec notfree alpha tau =
	match tau with
	| TYPE_var n ->
		n <> alpha
	| TYPE_func (tau1, tau2) ->
		(notfree alpha tau1) && (notfree alpha tau2)
	| TYPE_array (tau1, _)
	| TYPE_ref tau1 ->
		notfree alpha tau1
	| TYPE_int
	| TYPE_unit
	| TYPE_bool
	| TYPE_char
	| TYPE_float
	| TYPE_id _ ->
		true


let rec unifyAux map c =
	match c with
	| [] ->
		map
	| ((tau1, tau2), _) :: c when tau1 = tau2 ->
		unifyAux map c
	| ((TYPE_var alpha, tau), _) :: c
	| ((tau, TYPE_var alpha), _) :: c when notfree alpha tau ->
		let newMap = IntMap.add alpha tau map in
		let newC = subc alpha tau c in
		unifyAux newMap newC
	| ((TYPE_func (tau11, tau12), TYPE_func (tau21, tau22)), msg) :: c ->
		let newC = ((tau11, tau21), msg) :: ((tau12, tau22), msg) :: c in
		unifyAux map newC
	| ((TYPE_ref tau1, TYPE_ref tau2), msg) :: c ->
		let newC = ((tau1, tau2), msg) :: c in
		unifyAux map newC
	| ((TYPE_array (tau1, dim1), TYPE_array (tau2, dim2)), msg) :: c when dim1 = dim2 ->
		let newC = ((tau1, tau2), msg) :: c in
		unifyAux map newC
	| ((_, _), (pos, tau1, tau2)) :: c ->
		ErrorMsgs.errorMsg pos (sub map tau1) (sub map tau2);
		map


let unify c =
	let map = unifyAux IntMap.empty c in
	let process key _ oldMap =
		let clearTau = sub oldMap (TYPE_var key) in
		IntMap.add key clearTau oldMap
	in
	IntMap.fold process map map


let clearType initTyp pos =
	let rec clearTypeAux typ =
		match typ with
		| TYPE_var n ->
			ErrorMsgs.noPolyMsg pos initTyp
		| TYPE_array (typ, _)
		| TYPE_ref typ ->
			clearTypeAux typ
		| TYPE_func (typ1, typ2) ->
			clearTypeAux typ1;
			clearTypeAux typ2
		| _ ->
			()
	in
		clearTypeAux initTyp


let rec typeinfer (AST_program dl) c =
	let map = unify c in
	let dict typ =
		match typ with
		| TYPE_var n -> begin
			try IntMap.find n map with Not_found -> typ
		  end
		| _ ->
			typ
	in
	let newDl = List.map (def_tdef_construct dict) dl in
	AST_program newDl


and def_tdef_construct dict d =
	match d with
	| AST_letdef letdef ->
		let newLetdef = letdef_construct dict letdef in
		AST_letdef newLetdef
	| AST_typedef _ ->
		d


and letdef_construct dict (AST_let (recursive, defl)) =
	let newDefl = List.map (def_construct dict) defl in
	AST_let (recursive, newDefl)


and def_construct dict def =
	let par_construct (AST_par (name, typ)) (resTyp, acc) =
		let newTyp = dict typ in
		(TYPE_func (newTyp, resTyp), (AST_par (name, newTyp)) :: acc)
	in
	match def.de with
	| AST_def_normal (name, parl, typ, expr) ->
		let newTyp = dict typ in
		let (wholeTyp, newParl) = List.fold_right par_construct parl (newTyp, []) in
		clearType wholeTyp def.dpos;
		let newExpr = expr_construct dict expr in
		{
			de = AST_def_normal (name, newParl, newTyp, newExpr);
			dpos = def.dpos
		}
	| AST_def_mutable (name, exprl, typ) ->
		let newExprl = List.map (expr_construct dict) exprl in
		let newTyp = dict typ in
		clearType newTyp def.dpos;
		{
			de = AST_def_mutable (name, newExprl, newTyp);
			dpos = def.dpos
		}


and expr_construct dict expression =
	match expression.ex with
	| AST_expr_letdef_in (letdef, expr) ->
		let newLetdef = letdef_construct dict letdef in
		let newExpr = expr_construct dict expr in
		{
			ex = AST_expr_letdef_in (newLetdef, newExpr); 
			epos = expression.epos
		} 
	| AST_expr_unop (unop, expr) ->
		let newExpr = expr_construct dict expr in
		{
			ex = AST_expr_unop (unop, newExpr);
			epos = expression.epos
		}
	| AST_expr_binop (expr1, binop, expr2) ->
		let newExpr1 = expr_construct dict expr1 in
		let newExpr2 = expr_construct dict expr2 in
		{
			ex = AST_expr_binop (newExpr1, binop, newExpr2);
			epos = expression.epos
		}
	| AST_expr_if (expr1, expr2, expr3) ->
		let newExpr1 = expr_construct dict expr1 in
		let newExpr2 = expr_construct dict expr2 in
		let newExpr3 = expr_construct dict expr3 in
		{
			ex = AST_expr_if (newExpr1, newExpr2, newExpr3);
			epos = expression.epos
		}
	| AST_expr_while (expr1, expr2) ->
		let newExpr1 = expr_construct dict expr1 in
		let newExpr2 = expr_construct dict expr2 in
		{
			ex = AST_expr_while (newExpr1, newExpr2);
			epos = expression.epos
		}
	| AST_expr_for (name, expr1, down, expr2, expr3) ->
		let newExpr1 = expr_construct dict expr1 in
		let newExpr2 = expr_construct dict expr2 in
		let newExpr3 = expr_construct dict expr3 in
		{
			ex = AST_expr_for (name, newExpr1, down, newExpr2, newExpr3);
			epos = expression.epos
		}
	| AST_expr_match (expr, clausel) ->
		let newExpr = expr_construct dict expr in
		let newClausel = List.map (clause_construct dict) clausel in
		{
			ex = AST_expr_match (newExpr, newClausel);
			epos = expression.epos
		}
	| AST_expr_matrix_element (name, exprl) ->
		let newExprl = List.map (expr_construct dict) exprl in
		{
			ex = AST_expr_matrix_element (name, newExprl);
			epos = expression.epos
		}
	| AST_expr_function_call (name, exprl) ->
		let newExprl = List.map (expr_construct dict) exprl in
		{
			ex = AST_expr_function_call (name, newExprl);
			epos = expression.epos
		}
	| AST_expr_constructor_call (name, exprl) ->
		let newExprl = List.map (expr_construct dict) exprl in
		{
			ex = AST_expr_constructor_call (name, newExprl);
			epos = expression.epos
		}
	| AST_expr_name _
	| AST_expr_Name _
	| AST_expr_dim _
	| AST_expr_int_const _
	| AST_expr_float_const _
	| AST_expr_char_const _
	| AST_expr_string_const _
	| AST_expr_true
	| AST_expr_false
	| AST_expr_unit
	| AST_expr_new _
	| AST_expr_delete _ ->
		expression


and clause_construct dict (AST_clause (pat, expr)) =
	let newExpr = expr_construct dict expr in
	AST_clause (pat, newExpr)


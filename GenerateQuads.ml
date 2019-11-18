open Symbol
open Quads
open Types
open Ast
open Error
open ErrorMsgs
open LibraryFunctions
open Structs


(* apax kai to condition = true kata tin dhmiourgia twn quads mias ekfrasis 
   tote auti tha exei sigoura tupo bool kai tha epistrepsei true false listes *)


(* kata ton orimso mias sunartisis xekiname thetontas tailCall = true, twra an auto
   to true kataferei kai ftasei mexri ena function call tote auto tha einai tail call,
   paragodes pou boroun na kanoun tin timi tou tou tailCall = false einai gia paradeigma
   to function call na vrisketai sto expr1 tis ekfrasis expr1 + expr2 *)


(* san place epistrefw: 
   1) TailRecCall: an prokeite gia tail recursive call
   2) Unit: an prokeite gia ekfrasi tupou unit pou den einai TailRecCall
   3) TFLists: an to condition = true
   4) Place: an den einai kamia apo tis parapanw periptwseis *)


(* mai prwti skepsi tha htan molis kleinei ena scope 
   na kanw free ta entries tou omws tote endexetai kapoio 
   entry na ginei free kai katopin na ekxoritai sto apotelesma 
   mias sunartisis i se mia metavliti *)


let freeFunctionsAllocations f =
	let freeAllocatedEntry e =
		match e.entry_info with
		| ENTRY_variable inf ->
			if (inf.variable_allocated) then (
				let tau = getEntryTyp e in
				let entry_delete = newDeleteFunction tau in
				genQuad (Q_call (Left_entry entry_delete, [Par (OP_left (Left_entry e))], None))
			)
		| _ ->
			()
	in
	match f.entry_info with
	| ENTRY_function inf ->
		List.iter freeAllocatedEntry inf.function_varlist
	| _ ->
		internal "It should be a function";
		raise Exit


let rec generateQuads (AST_program dl) =
	initSymbolTable 256;
	let libFuncs = addLibrary () in
	let outer_func = newFunctionInitStep "_outer" (TYPE_func(TYPE_unit, TYPE_unit)) TYPE_unit true in
	newFunctionMidStep outer_func;
	genQuad (Q_unit outer_func);
	List.iter (def_tdef_generate outer_func libFuncs) dl;
	newFunctionLastStep outer_func;
	List.iter (fun _ -> closeScope ()) dl;
	freeFunctionsAllocations outer_func;
	genQuad (Q_endu outer_func);
	finalizeQuads ()


and def_tdef_generate outer_func libFuncs d =
	match d with
	| AST_letdef letdef ->
		letdef_generate outer_func libFuncs letdef
	| AST_typedef _ ->
		openScope ();
		ErrorMsgs.noUdtTypes ()


and letdef_generate outer_func libFuncs (AST_let (recursive, defl)) =
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
	List.iter (def_generate outer_func libFuncs) defXentryList;
	hideScope !currentScope false


and def_generate outer_func libFuncs (def, ent) =
	let addPars func_ent parl =
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
			(* prokeimenou tuxon almata se !nextQuad na
			  min phgainoun sto Q_unit tis epomenis sunartisis *)
			genQuad quad_dummy;
			genQuad (Q_unit ent);
			addPars ent parl;
			let (t, p) = expr_generate ent libFuncs false true expr in
			match p with
			| Place p ->
				genQuad (Q_return p);
				newFunctionLastStep ent;
				closeScope ();
				freeFunctionsAllocations ent;
				genQuad (Q_endu ent)
			| Unit ->
				newFunctionLastStep ent;
				closeScope ();
				freeFunctionsAllocations ent;
				genQuad (Q_endu ent)
			| TailRecCall ->
				(* sunartisi llama pou kanei loop forever, exei mono anadromikes klhseis *)
				ErrorMsgs.loopForeverMsg def.dpos name;
				newFunctionLastStep ent;
				closeScope ();
				freeFunctionsAllocations ent;
				genQuad (Q_endu ent)
			| TFLists _ ->
				internal "It should not be a condition";
				raise Exit
		)
		else (
			if (typ = TYPE_unit) then (
				ignore (expr_generate outer_func libFuncs false false expr)
			)
			else (
				let (_, p) = expr_generatePLACE outer_func libFuncs false expr in
				genQuad (Q_assign (p, Left_entry ent))
			)
		)
	| AST_def_mutable (_, exprl, derefTyp) ->
		if (exprl <> []) then (
			let process expr =
				let (t, p) = expr_generatePLACE outer_func libFuncs false expr in
				p
			in
			let dimPlaces = List.map process exprl in
			let dimPars = List.map (fun d -> Par d) dimPlaces in
			let dims = List.length exprl in
			let typ = getEntryTyp ent in
			let size = sizeOfType derefTyp in
			let entry_make_array = newMakeArrayFunction typ in
			let par1 = Par (OP_constant (Const_int size)) in
			let par2 = Par (OP_constant (Const_int dims)) in
			let res = Some (Left_entry ent) in
			genQuad (Q_call (Left_entry entry_make_array, dimPars @ [par1; par2], res))
		)
		else (
			let typ = getEntryTyp ent in
			let size = sizeOfType derefTyp in
			let entry_new = newNewFunction typ in
			let par = Par (OP_constant (Const_int size)) in
			let res = Some (Left_entry ent) in
			genQuad (Q_call (Left_entry entry_new, [par], res))
		)


and expr_generate outer_func libFuncs condition tailCall expr =
	match expr.ex with
	| AST_expr_name name ->
		let e = lookupEntry name LOOKUP_ALL_SCOPES true in
		let tau = getNameTyp name in
		if (condition) then (
			let tList = [genQuadRet (Q_ifb (OP_left (Left_entry e), star))] in
			let fList = [genQuadRet (Q_jump star)] in
			(TYPE_bool, TFLists (tList, fList))
		)
		else if (tau = TYPE_unit) then (
			(TYPE_unit, Unit)
		)
		else (
			(tau, Place (OP_left (Left_entry e)))
		)
	| AST_expr_int_const n ->
		(TYPE_int, Place (OP_constant (Const_int n)))
	| AST_expr_float_const f ->
		(TYPE_float, Place (OP_constant (Const_float f)))
	| AST_expr_char_const c ->
		(TYPE_char, Place (OP_constant (Const_char c)))
	| AST_expr_string_const s ->
		(TYPE_array (TYPE_char, 1), Place (OP_constant (Const_string s)))
	| AST_expr_true ->
		if (condition) then (
			let tList = [genQuadRet (Q_jump star)] in
			(TYPE_bool, TFLists (tList, []))
		)
		else (
			(TYPE_bool, Place (OP_constant (Const_bool true)))
		)
	| AST_expr_false ->
		if (condition) then (
			let fList = [genQuadRet (Q_jump star)] in
			(TYPE_bool, TFLists ([], fList))
		)
		else (
			(TYPE_bool, Place (OP_constant (Const_bool false)))
		)
	| AST_expr_unit ->
		(TYPE_unit, Unit)
	| AST_expr_binop (expr1, bn, expr2) ->
		binop_generate outer_func libFuncs condition tailCall (expr1, bn, expr2)
	| AST_expr_unop (un, expr) ->
		unop_generate outer_func libFuncs condition tailCall (un, expr)
	| AST_expr_dim (d, name) -> begin
		let typ = getNameTyp name in
		let entry_array = lookupEntry name LOOKUP_ALL_SCOPES true in
		match typ with
		| TYPE_array _ ->
			let t = newTemporary TYPE_int outer_func in
			genQuad (Q_dim (entry_array, d, Left_entry t));
			(TYPE_int, Place (OP_left (Left_entry t)))
		| _ ->
			internal "It should be of type array";
			raise Exit
	  end
	| AST_expr_matrix_element (name, exprl) -> begin
		let typ = getNameTyp name in
		let entry_array = lookupEntry name LOOKUP_ALL_SCOPES true in
		match typ with
		| TYPE_array (tau, dim) ->
			let process expr =
				let (_, p) = expr_generatePLACE outer_func libFuncs false expr in
				p
			in
			let dimPlaces = List.map process exprl in
			let firstDimPlace = List.hd dimPlaces in
			let w = newTemporary (TYPE_ref tau) outer_func in
			if (dim = 1) then (
				genQuad (Q_array (entry_array, firstDimPlace, Left_entry w))
			)
			else (
				let index = getSafeLeftEntry firstDimPlace TYPE_int outer_func in
				let dimPlaces = List.tl dimPlaces in
				let t = newTemporary TYPE_int outer_func in
				let process d dPlace = 
					genQuad (Q_dim (entry_array, d, Left_entry t));
					genQuad (Q_calculate (OP_left (Left_entry index), MUL, OP_left (Left_entry t), Left_entry index));
					genQuad (Q_calculate (OP_left (Left_entry index), ADD, dPlace, Left_entry index));
					d + 1
				in
				ignore (List.fold_left process 2 dimPlaces);
				genQuad (Q_array (entry_array, OP_left (Left_entry index), Left_entry w))
			);
			(TYPE_ref tau, Place (OP_left (Left_entry w)))
		| _ ->
			internal "It should be of type array";
			raise Exit
	  end
	| AST_expr_if (expr1, expr2, expr3) when (expr3.ex = AST_expr_unit) -> begin
		let (_, (tList1, fList1)) = expr_generateLISTS outer_func libFuncs expr1 in
		patch tList1 !nextQuad;
		let (t2, p2) = expr_generate outer_func libFuncs condition tailCall expr2 in
		match p2 with
		| Unit ->
			patch fList1 !nextQuad;
			(TYPE_unit, Unit)
		| TailRecCall ->
			patch fList1 !nextQuad;
			(TYPE_unit, Unit)
		| TFLists _
		| Place _ ->
			internal "It should be of type unit";
			raise Exit
	  end
	| AST_expr_if (expr1, expr2, expr3) -> begin
		let (_, (tList1, fList1)) = expr_generateLISTS outer_func libFuncs expr1 in
		patch tList1 !nextQuad;
		let (t2, p2) = expr_generate outer_func libFuncs condition tailCall expr2 in
		match p2 with
		| TFLists (tList2, fList2) ->
			patch fList1 !nextQuad;
			let (_, (tList3, fList3)) = expr_generateLISTS outer_func libFuncs expr3 in
			(TYPE_bool, TFLists (tList2 @ tList3, fList2 @ fList3))
		| Place p2 -> begin
			let w = getSafeLeftEntry p2 t2 outer_func in
			let bypassElseQdRef = genQuadRet (Q_jump star) in
			patch fList1 !nextQuad;
			let (_, p3) = expr_generate outer_func libFuncs false tailCall expr3 in
			match p3 with
			| Place p3 ->
				genQuad (Q_assign (p3, Left_entry w));
				patch [bypassElseQdRef] !nextQuad;
				(t2, Place (OP_left (Left_entry w)))
			| TailRecCall ->
				patch [bypassElseQdRef] !nextQuad;
				(t2, Place (OP_left (Left_entry w)))
			| Unit
			| TFLists _ ->
				internal "It should not be of type unit nor a condition";
				raise Exit
		  end
		| Unit -> begin
			let bypassElseQdRef = genQuadRet (Q_jump star) in
			patch fList1 !nextQuad;
			let (_, p3) = expr_generate outer_func libFuncs false tailCall expr3 in
			match p3 with
			| Unit ->
				patch [bypassElseQdRef] !nextQuad;
				(TYPE_unit, Unit)
			| TailRecCall ->
				patch [bypassElseQdRef] !nextQuad;
				(* epistrefoume san place to unit gia na to xexorisoume apo tin
				   periptwsi pou exoume ton orismo mias sunartisis pou sigoura kanei
				   loop forever gia kathe eisodo *)
				(TYPE_unit, Unit)
			| _ ->
				internal "It should be o type unit";
				raise Exit
		  end
		| TailRecCall -> begin
			patch fList1 !nextQuad;
			let (_, p3) = expr_generate outer_func libFuncs false tailCall expr3 in
			match p3 with
			| Place p3 ->
				(t2, Place p3)
			| Unit ->
				(t2, Unit)
			| TailRecCall ->
				(t2, TailRecCall)
			| TFLists _ ->
				internal "It should not be a condition";
				raise Exit
		  end
	  end
	| AST_expr_while (expr1, expr2) ->
		let (_, (tListStart, fListStart)) = expr_generateLISTS outer_func libFuncs expr1 in
		let q = !nextQuad in
		ignore (expr_generate outer_func libFuncs false false expr2);
		let (_, (tListEnd, fListEnd)) = expr_generateLISTS outer_func libFuncs expr1 in
		patch (tListStart @ tListEnd) q;
		patch (fListStart @ fListEnd) !nextQuad;
		(TYPE_unit, Unit)
	| AST_expr_for (name, expr1, down, expr2, expr3) ->
		let (_, p1) = expr_generatePLACE outer_func libFuncs false expr1 in
		let (_, p2) = expr_generatePLACE outer_func libFuncs false expr2 in
		openScope ();
		let i = newVariable name TYPE_int outer_func false in
		let op_i = OP_left (Left_entry i) in
		genQuad (Q_assign (p1, Left_entry i));
		let rel = if (not down) then GE else LE in
		let outQdRef = genQuadRet (Q_compare (op_i, rel, p2, star)) in
		let q = !nextQuad in
		ignore (expr_generate outer_func libFuncs false false expr3);
		let oper = if (not down) then ADD else SUB in
		genQuad (Q_calculate (op_i, oper, OP_constant (Const_int 1), Left_entry i));
		let rel = if (not down) then LEQ else GEQ in
		genQuad(Q_compare (op_i, rel, p2, q));
		patch [outQdRef] !nextQuad;
		closeScope ();
		(TYPE_unit, Unit)
	| AST_expr_new typ ->
		(* to deallocation einai euthini tou programmatisti *)
		let entry_new = newNewFunction typ in
		let w = newTemporary (TYPE_ref typ) outer_func in
		let par = Par (OP_constant (Const_int (sizeOfType typ))) in
		let res = Some (Left_entry w) in
		genQuad (Q_call (Left_entry entry_new, [par], res));
		(TYPE_ref typ, Place (OP_left (Left_entry w)))
	| AST_expr_delete expr ->
		let (t, p) = expr_generatePLACE outer_func libFuncs false expr in
		let entry_delete = newDeleteFunction t in
		genQuad (Q_call (Left_entry entry_delete, [Par p], None));
		(TYPE_unit, Unit)
	| AST_expr_letdef_in (letdef, expr) ->
		letdef_generate outer_func libFuncs letdef;
		let (t, p) = expr_generate outer_func libFuncs condition tailCall expr in
		closeScope ();
		(t, p)
	| AST_expr_function_call (name, exprl) ->
		let getPlace p =
			match p with
			| Place p ->
				p
			| _ ->
				internal "It should be a place";
				raise Exit
		in
		let process expr acc =
			let (t, p) = expr_generate outer_func libFuncs false false expr in
			if (t <> TYPE_unit) then (
				(Par (getPlace p)) :: acc
			)
			else (
				acc
			)
		in
		let parl = List.fold_right process exprl [] in
		let entry_f = lookupEntry name LOOKUP_ALL_SCOPES true in
		let resTyp = getNameResTyp name in
		let tailRecCall = (tailCall && (name = outer_func.entry_name)) in
		if (tailRecCall) then (
			genQuad (Q_tailRecCall (outer_func, parl));
			(resTyp, TailRecCall)
		)
		else (
			if (resTyp = TYPE_unit) then (
				genQuad (Q_call (Left_entry entry_f, parl, None));
				(TYPE_unit, Unit)
			)
			else (
				let w = newTemporary resTyp outer_func in
				genQuad (Q_call (Left_entry entry_f, parl, Some (Left_entry w)));
				if (condition) then (
					let tList = [genQuadRet (Q_ifb (OP_left (Left_entry w), star))] in
					let fList = [genQuadRet (Q_jump star)] in
					(TYPE_bool, TFLists (tList, fList))
				)
				else (
					(resTyp, Place (OP_left (Left_entry w)))
				)
			)
		)
	| AST_expr_Name _
	| AST_expr_constructor_call _
	| AST_expr_match _ ->
		ErrorMsgs.noUdtTypes ();
		(TYPE_id "nonde", Unit)


and binop_generate outer_func libFuncs condition tailCall (expr1, bn, expr2) =
	let (entry_ln, entry_exp) = libFuncs in
	let operator_of_binop binop = 
		match binop with
		| Fadd ->
			FADD
		| Add ->
			ADD
		| Fsub ->
			FSUB
		| Sub ->
			SUB
		| Fmul ->
			FMUL
		| Mul ->
			MUL
		| Fdiv ->
			FDIV
		| Div ->
			DIV
		| Mod ->
			MOD
		| _ ->
			internal "Operator of binop not available";
			raise Exit
	in
	let relation_of_binop typ binop =
		match binop with
		| Naturaleq
		| Structeq ->
			if (typ = TYPE_float) then FEQ
			else if (typ = TYPE_char) then CEQ
			else EQ
		| Naturalneq
		| Structneq ->
			if (typ = TYPE_float) then FNEQ
			else if (typ = TYPE_char) then CNEQ
			else NEQ
		| Less ->
			if (typ = TYPE_float) then FLE
			else if (typ = TYPE_char) then CLE
			else LE
		| Greater ->
			if (typ = TYPE_float) then FGE
			else if (typ = TYPE_char) then CGE
			else GE
		| Leq ->
			if (typ = TYPE_float) then FLEQ
			else if (typ = TYPE_char) then CLEQ
			else LEQ
		| Geq ->
			if (typ = TYPE_float) then FGEQ
			else if (typ = TYPE_char) then CGEQ
			else GEQ
	 	| _ ->
			internal "Relation of binop not available";
			raise Exit
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
	| Fdiv ->
		let (t1, p1) = expr_generatePLACE outer_func libFuncs false expr1 in
		let (_, p2) = expr_generatePLACE outer_func libFuncs false expr2 in
		let oper = operator_of_binop bn in
		let w = getSafeLeftEntry2Places p1 p2 t1 outer_func in
		genQuad (Q_calculate (p1, oper, p2, Left_entry w));
		(t1, Place (OP_left (Left_entry w)))
	| Power ->
		let (t1, p1) = expr_generatePLACE outer_func libFuncs false expr1 in
		let (_, p2) = expr_generatePLACE outer_func libFuncs false expr2 in
		let w = getSafeLeftEntry p1 t1 outer_func in
		genQuad (Q_call (Left_entry entry_ln, [Par p1], Some (Left_entry w)));
		genQuad (Q_calculate (OP_left (Left_entry w), FMUL, p2, Left_entry w));
		genQuad (Q_call (Left_entry entry_exp, [Par (OP_left (Left_entry w))], Some (Left_entry w)));
		(t1, Place (OP_left (Left_entry w)))
	| And ->
		if (condition) then (
			let (_, (tList1, fList1)) = expr_generateLISTS outer_func libFuncs expr1 in
			patch tList1 !nextQuad;
			let (_, (tList2, fList2)) = expr_generateLISTS outer_func libFuncs expr2 in
			(TYPE_bool, TFLists (tList2, fList1 @ fList2))
		)
		else (
			let (t1, p1) = expr_generatePLACE outer_func libFuncs false expr1 in
			let (_, p2) = expr_generatePLACE outer_func libFuncs false expr2 in
			let w = getSafeLeftEntry2Places p1 p2 t1 outer_func in
			let trueQdRef1 = genQuadRet (Q_ifb (p1, star)) in
			genQuad (Q_assign (OP_constant (Const_bool false), Left_entry w));
			let falseQdRef1 = genQuadRet (Q_jump star) in
			patch [trueQdRef1] !nextQuad;
			let trueQdRef2 = genQuadRet (Q_ifb (p2, star)) in
			genQuad (Q_assign (OP_constant (Const_bool false), Left_entry w));
			let falseQdRef2 = genQuadRet (Q_jump star) in
			patch [trueQdRef2] !nextQuad;
			genQuad (Q_assign (OP_constant (Const_bool true), Left_entry w));
			patch [falseQdRef1; falseQdRef2] !nextQuad;
			(TYPE_bool, Place (OP_left (Left_entry w)))
		)	
	| Or ->
		if (condition) then (
			let (_, (tList1, fList1)) = expr_generateLISTS outer_func libFuncs expr1 in
			patch fList1 !nextQuad;
			let (_, (tList2, fList2)) = expr_generateLISTS outer_func libFuncs expr2 in
			(TYPE_bool, TFLists (tList1 @ tList2, fList2))
		)
		else (
			let (t1, p1) = expr_generatePLACE outer_func libFuncs false expr1 in
			let (_, p2) = expr_generatePLACE outer_func libFuncs false expr2 in
			let w = getSafeLeftEntry2Places p1 p2 t1 outer_func in
			let trueQdRef1 = genQuadRet (Q_ifb (p1, star)) in
			genQuad (Q_ifb (p2, !nextQuad + 3));
			genQuad (Q_assign (OP_constant (Const_bool false), Left_entry w));
			genQuad (Q_jump (!nextQuad + 2));
			patch [trueQdRef1] !nextQuad;
			genQuad (Q_assign (OP_constant (Const_bool true), Left_entry w));
			(TYPE_bool,Place (OP_left (Left_entry w)))
		)
	| Semicolumn ->
		ignore (expr_generate outer_func libFuncs false false expr1);
		let (t2, p2) = expr_generate outer_func libFuncs condition tailCall expr2 in
		(t2, p2)
	| Naturaleq
	| Structeq 
	| Naturalneq
	| Structneq
	| Less
	| Greater
	| Leq
	| Geq -> begin
		let (t1, p1) = expr_generate outer_func libFuncs false false expr1 in
		match p1 with
		| Place p1 ->
			let (_, p2) = expr_generatePLACE outer_func libFuncs false expr2 in
			let rel = relation_of_binop t1 bn in
			let tList = [genQuadRet (Q_compare (p1, rel, p2, star))] in
			if (condition) then (
				let fList = [genQuadRet (Q_jump star)] in
				(TYPE_bool, TFLists (tList, fList))
			)
			else (
				let w = newTemporary TYPE_bool outer_func in
				genQuad (Q_assign (OP_constant (Const_bool false), Left_entry w));
				genQuad (Q_jump (!nextQuad + 2));
				patch tList !nextQuad;
				genQuad (Q_assign (OP_constant (Const_bool true), Left_entry w));
				(TYPE_bool, Place (OP_left (Left_entry w)))
			)
		| Unit ->
			ignore (expr_generate outer_func libFuncs false false expr1);
			if (condition) then (
				if ((bn = Naturaleq) || (bn =  Structeq)) then (
					let tList = [genQuadRet (Q_jump star)] in
					(TYPE_bool, TFLists (tList, []))
				)
				else (
					let fList = [genQuadRet (Q_jump star)] in
					(TYPE_bool, TFLists ([], fList))
				)
			)
			else (
				if ((bn = Naturaleq) || (bn =  Structeq)) then (
					(TYPE_bool, Place (OP_constant (Const_bool true)))
				)
				else (
					(TYPE_bool, Place (OP_constant (Const_bool false)))
				)
			)
		| TFLists _
		| TailRecCall ->
			internal "It should be either unit or a place";
			raise Exit
	  end
 	| Assign ->
		let (_, p2) = expr_generate outer_func libFuncs false false expr2 in
		match p2 with
		| Place p2 ->
			let (t1, p1) = expr_generatePLACE outer_func libFuncs false expr1 in
			let w_entry = getLeftEntry p1 t1 outer_func in
			genQuad (Q_assign (p2, Left_deref w_entry));
			(TYPE_unit, Unit)
		| Unit ->
			ignore (expr_generate outer_func libFuncs false false expr1);
			(TYPE_unit, Unit)
		| TFLists _
		| TailRecCall ->
			internal "It should be a place or unit";
			raise Exit


and unop_generate outer_func libFuncs condition tailCall (un, expr) =
	match un with
	| Positive
	| Fpositive ->
		let (t, p) =  expr_generatePLACE outer_func libFuncs tailCall expr in
		(t, Place p)
	| Negative ->
		let (t, p) =  expr_generatePLACE outer_func libFuncs false expr in
		let w = newTemporary t outer_func in
		genQuad (Q_calculate (OP_constant Const_int 0, SUB, p, Left_entry w));
		(t, Place (OP_left (Left_entry w)))
	| Fnegative ->
		let (t, p) =  expr_generatePLACE outer_func libFuncs false expr in
		let w = newTemporary t outer_func in
		genQuad (Q_calculate (OP_constant Const_float 0.0, SUB, p, Left_entry w));
		(t, Place (OP_left (Left_entry w)))
	| Not ->
		if (condition) then (
			let (t, (tList, fList)) = expr_generateLISTS outer_func libFuncs expr in
			(t, TFLists (fList, tList))
		)
		else (
			let (t, p) =  expr_generatePLACE outer_func libFuncs false expr in
			let w = newTemporary t outer_func in
			genQuad (Q_ifb (p, !nextQuad + 3));
			genQuad (Q_assign (OP_constant (Const_bool true), Left_entry w));
			genQuad (Q_jump (!nextQuad + 2));
			genQuad (Q_assign (OP_constant (Const_bool false), Left_entry w));
			(t, Place (OP_left (Left_entry w)))
		)
	| Dereference -> begin
		let (t, p) =  expr_generatePLACE outer_func libFuncs false expr in
		let w = getLeftEntry p t outer_func in
		match t with
		| TYPE_ref tau ->
			if (condition) then (
				let tList = [genQuadRet (Q_ifb (OP_left (Left_deref w), star))] in
				let fList = [genQuadRet (Q_jump star)] in
				(tau, TFLists (tList, fList))
			)
			else if (tau = TYPE_unit) then (
				(TYPE_unit, Unit)
			)
			else(
				(tau, Place (OP_left (Left_deref w)))
			)
		| _ ->
			internal "It should be a reference";
			raise Exit
	  end


and expr_generatePLACE outer_func libFuncs tailCall expr =
	let (t, p) = expr_generate outer_func libFuncs false tailCall expr in
	match p with
	| Place p ->
		(t, p)
	| TFLists _
	| Unit
	| TailRecCall ->
		internal "It should be a place";
		raise Exit


and expr_generateLISTS outer_func libFuncs expr =
	let (t, p) = expr_generate outer_func libFuncs true false expr in
	match p with
	| TFLists (tList, fList) ->
		(t, (tList, fList))
	| Place _
	| Unit
	| TailRecCall ->
		internal "It should be a condition";
		raise Exit


and getSafeLeftEntry p typ outer_func =
	match p with
	| OP_left (Left_entry  e) -> begin
		match e.entry_info with
		| ENTRY_temporary _ ->
			e
		| _ ->
			let w = newTemporary typ outer_func in
			genQuad (Q_assign (p, Left_entry w));
			w
	  end
	| _ ->
		let w = newTemporary typ outer_func in
		genQuad (Q_assign (p, Left_entry w));
		w


and getLeftEntry p typ outer_func =
	match p with
	| OP_left (Left_entry e) ->
		e
	| _ ->
		let w = newTemporary typ outer_func in
		genQuad (Q_assign (p, Left_entry w));
		w


and getSafeLeftEntry2Places p1 p2 typ outer_func =
	match p1 with
	| OP_left (Left_entry e1) -> begin
		match e1.entry_info with
		| ENTRY_temporary _ ->
			e1
		| _ ->
			getSafeLeftEntry2PlacesAux p2 typ outer_func
	  end
	| _ ->
		getSafeLeftEntry2PlacesAux p2 typ outer_func


and getSafeLeftEntry2PlacesAux p2 typ outer_func =
	match p2 with
	| OP_left (Left_entry e2) -> begin
		match e2.entry_info with
		| ENTRY_temporary _ ->
			e2
		| _ ->
			let w = newTemporary typ outer_func in
			w
	  end
	| _ ->
		let w = newTemporary typ outer_func in
		w



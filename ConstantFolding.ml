open Quads
open Error


let executeComparison c1 rel c2 =
	match rel with
	| EQ 
	| FEQ
	| CEQ ->
		c1 = c2
	| NEQ 
	| FNEQ
	| CNEQ ->
		c1 <> c2
	| GE
	| FGE
	| CGE ->
		c1 > c2
	| LE
	| FLE
	| CLE ->
		c1 < c2
	| GEQ
	| FGEQ
	| CGEQ ->
		c1 >= c2
	| LEQ
	| FLEQ
	| CLEQ ->
		c1 <= c2


let executeInt d1 oper d2 =
	match oper with
	| ADD ->
		OP_constant (Const_int (d1 + d2))
	| SUB ->
		OP_constant (Const_int (d1 - d2))
	| MUL ->
		OP_constant (Const_int (d1 * d2))
	| DIV ->
		OP_constant (Const_int (d1 / d2))
	| MOD ->
		OP_constant (Const_int (d1 mod d2))
	| _ ->
		internal "It should be an operator on integers";
		raise Exit


let executeReal f1 oper f2 =
	match oper with
	| FADD ->
		OP_constant (Const_float (f1 +. f2))
	| FSUB ->
		OP_constant (Const_float (f1 -. f2))
	| FMUL ->
		OP_constant (Const_float (f1 *. f2))
	| FDIV ->
		OP_constant (Const_float (f1 /. f2))
	| _ ->
		internal "It should be an operator on floats";
		raise Exit


let constantFolding (blocks, succs, preds) =
	let blockFolding (i, initChanged) block =
		let quadFolding (j, changed)(qLabel, quad) =
			let fixGraphEdges1 tag =
				if (tag <> qLabel + 1) then (
					succs.(i) <- List.filter (fun b -> b <> i + 1) succs.(i);
					preds.(i + 1) <- List.filter (fun b -> b <> i) preds.(i + 1)
				)
			in
			let fixGraphEdges2 tag =
				if (tag <> qLabel + 1) then (
					let b_tag = List.find (fun b -> b <> i + 1) succs.(i) in
					succs.(i) <- List.filter (fun b -> b <> b_tag) succs.(i);
					preds.(b_tag) <- List.filter (fun b -> b <> i) preds.(b_tag)
				)
			in
			match quad with
			| Q_ifb (OP_constant (Const_bool true), t) ->
				block.(j) <- (qLabel, Q_jump t);
				fixGraphEdges1 t;
				(j + 1, changed)
			| Q_ifb (OP_constant (Const_bool false), t) ->
				block.(j) <- (qLabel, quad_dummy);
				fixGraphEdges2 t;
				(j + 1, changed)
			| Q_compare (OP_constant (Const_int d1), rel, OP_constant (Const_int d2), t) ->
				if (executeComparison d1 rel d2) then (
					block.(j) <- (qLabel, Q_jump t);
					fixGraphEdges1 t
				)
				else (
					block.(j) <- (qLabel, quad_dummy);
					fixGraphEdges2 t
				);
				(j + 1, changed)
			| Q_compare (OP_constant (Const_float f1), rel, OP_constant (Const_float f2), t) ->
				if (executeComparison f1 rel f2) then (
					block.(j) <- (qLabel, Q_jump t);
					fixGraphEdges1 t
				)
				else (
					block.(j) <- (qLabel, quad_dummy);
					fixGraphEdges2 t
				);
				(j + 1, changed)
			| Q_compare (OP_constant (Const_char c1), rel, OP_constant (Const_char c2), t) ->
				if (executeComparison c1 rel c2) then (
					block.(j) <- (qLabel, Q_jump t);
					fixGraphEdges1 t
				)
				else (
					block.(j) <- (qLabel, quad_dummy);
					fixGraphEdges2 t
				);
				(j + 1, changed)
			| Q_compare (OP_constant (Const_bool b1), rel, OP_constant (Const_bool b2), t) ->
				if (executeComparison b1 rel b2) then (
					block.(j) <- (qLabel, Q_jump t);
					fixGraphEdges1 t
				)
				else (
					block.(j) <- (qLabel, quad_dummy);
					fixGraphEdges2 t
				);
				(j + 1, changed)
			| Q_compare (OP_constant (Const_string s1), rel, OP_constant (Const_string s2), t) ->
				if (executeComparison s1 rel s2) then (
					block.(j) <- (qLabel, Q_jump t);
					fixGraphEdges1 t
				)
				else (
					block.(j) <- (qLabel, quad_dummy);
					fixGraphEdges2 t
				);
				(j + 1, changed)
			| Q_calculate (OP_constant (Const_int d1), oper, OP_constant (Const_int d2), l) ->
				let op = executeInt d1 oper d2 in
				block.(j) <- (qLabel, Q_assign (op, l));
				(j + 1, true)
			| Q_calculate (OP_constant (Const_float f1), oper, OP_constant (Const_float f2), l) ->
				let op = executeReal f1 oper f2 in
				block.(j) <- (qLabel, Q_assign (op, l));
				(j + 1, true)
			| Q_calculate (OP_constant (Const_int 0), ADD, op, l)
			| Q_calculate (op, ADD, (OP_constant (Const_int 0)), l) ->
				block.(j) <- (qLabel, Q_assign (op, l));
				(j + 1, true)
			| Q_calculate (OP_constant (Const_int 0), MUL, _, l)
			| Q_calculate (_, MUL, OP_constant (Const_int 0), l) ->
				block.(j) <- (qLabel, Q_assign (OP_constant (Const_int 0), l));
				(j + 1, true)
			| Q_calculate (OP_constant (Const_int 1), MUL, op, l)
			| Q_calculate (op, MUL, OP_constant (Const_int 1), l) ->
				block.(j) <- (qLabel, Q_assign (op, l));
				(j + 1, true)
			| Q_calculate (OP_constant (Const_float 0.0), FADD, op, l)
			| Q_calculate (op, FADD, OP_constant (Const_float 0.0), l) ->
				block.(j) <- (qLabel, Q_assign (op, l));
				(j + 1, true)
			| Q_calculate (OP_constant (Const_float 0.0), FMUL, _, l)
			| Q_calculate (_, FMUL, OP_constant (Const_float 0.0), l) ->
				block.(j) <- (qLabel, Q_assign (OP_constant (Const_float 0.0), l));
				(j + 1, true)
			| Q_calculate (OP_constant (Const_float 1.0), FMUL, op, l)
			| Q_calculate (op, FMUL, OP_constant (Const_float 1.0), l) ->
				block.(j) <- (qLabel,  Q_assign (op, l));
				(j + 1, true)
			| _ ->
				(j + 1, changed)
		in
		let (_, newChanged) = Array.fold_left quadFolding (0, initChanged) block in
		(i + 1, newChanged)
	in
	snd (Array.fold_left blockFolding (0, false) blocks)



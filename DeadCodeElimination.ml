open Symbol
open Quads
open Structs
open ConstructDuUdChains


let markEssential blocks mark =
	let markBlock (block_nr, initWorkList) block =
		let markQuad (i, wkList) (_, quad) =
			let essentialL l =
				match l with
				| Left_entry e -> begin
					match e.entry_info with
					| ENTRY_temporary _ ->
						false
					| _ ->
						true
				  end
				| Left_deref _ ->
					true
			in
			match quad with
			| Q_unit _
			| Q_endu _
			| Q_jump _
			| Q_ifb _
			| Q_compare _
			| Q_call _
			| Q_tailRecCall _
			| Q_return _
			| Q_array _ ->
				mark.(block_nr).(i) <- true;
				(i + 1, TupleSet.add (block_nr, i) wkList)
			| Q_dim (_, _, l)
			| Q_assign (_, l)
			| Q_calculate (_, _, _, l) ->
				if (essentialL l) then (
					mark.(block_nr).(i) <- true;
					(i + 1, TupleSet.add (block_nr, i) wkList)
				)
				else (
					(i + 1, wkList)
				)
		in
		mark.(block_nr) <- Array.make (Array.length block) false;
		let resWorkList = snd (Array.fold_left markQuad (0, initWorkList) block) in
		(block_nr + 1, resWorkList)
	in
	snd (Array.fold_left markBlock (0, TupleSet.empty) blocks)


let rec deadCodeElimination_aux blocks duChains udChains mark wkLst =
	let markUdChain op (i, j) workList =
		match op with
		| OP_left (Left_entry e)
		| OP_left (Left_deref e) -> begin
			try (
				let defs_e = TripleMap.find (e.entry_number, i, j) udChains in
				let expand wkLst (k, l) =
					if (mark.(k).(l)) then (
						wkLst
					)
					else (
						mark.(k).(l) <- true;
						TupleSet.add (k, l) wkLst
					)
				in
				List.fold_left expand workList defs_e
			) with Not_found ->
				workList
		  end
		| _ ->
			workList
	in
	let markDuChain l (i, j) workList =
		match l with
		| Left_entry e -> begin
			try (
				let uses_e = TripleMap.find (e.entry_number, i, j) duChains in
				let expand wkLst (k, l) =
					match (mark.(k).(l), snd blocks.(k).(l)) with
					| (true, _) ->
						wkLst
					| (false, Q_ifb _)
					| (false, Q_compare _) ->
						mark.(k).(l) <- true;
						TupleSet.add (k, l) wkLst
					| _ ->
						wkLst
				in
				List.fold_left expand workList uses_e
			) with Not_found ->
				workList
		  end
		| Left_deref e ->
			markUdChain (OP_left l) (i, j) workList
	in
	if (not (TupleSet.is_empty wkLst)) then (
		let (i, j) = TupleSet.choose wkLst in
		let workList = TupleSet.remove (i, j) wkLst in
		let newWorkList =
			let markPar wkLst (Par p) =
				markUdChain p (i, j) wkLst
			in
			match (snd blocks.(i).(j)) with
			| Q_unit _
			| Q_endu _
			| Q_jump _ ->
				workList
			| Q_assign (op, l) ->
				let workList1 = markUdChain op (i, j) workList in
				markDuChain l (i, j) workList1
			| Q_calculate (op1, _, op2, l) ->
				let workList1 = markUdChain op1 (i, j) workList in
				let workList2 = markUdChain op2 (i, j) workList1 in
				markDuChain l (i, j) workList2
			| Q_compare (op1, _, op2, _) ->
				let workList1 = markUdChain op1 (i, j) workList in
				markUdChain op2 (i, j) workList1
			| Q_array (e, op, l) ->
				let workList1 = markUdChain op (i, j) workList in
				let workList2 = markUdChain (OP_left (Left_entry e)) (i, j) workList1 in
				markDuChain l (i, j) workList2
			| Q_dim (e, _, l) ->
				let workList1 = markUdChain (OP_left (Left_entry e)) (i, j) workList in
				markDuChain l (i, j) workList1
			| Q_ifb (op, _)
			| Q_return op ->
				markUdChain op (i, j) workList
			| Q_call (l, parl, Some r) ->
				let workList1 = List.fold_left markPar workList parl in
				let workList2 = markUdChain (OP_left l) (i, j) workList1 in
				markDuChain r (i, j) workList2
			| Q_call (l, parl, None) ->
				let workList1 = List.fold_left markPar workList parl in
				markUdChain (OP_left l) (i, j) workList1
			| Q_tailRecCall (e, parl) ->
				let workList1 = List.fold_left markPar workList parl in
				markUdChain (OP_left (Left_entry e)) (i, j) workList1
		in
		deadCodeElimination_aux blocks duChains udChains mark newWorkList
	)


let deleteUnmarkedQuads blocks mark =
	let processBlock i block =
		let processQuad j quad =
			if (not mark.(i).(j)) then block.(j) <- (fst block.(j), quad_dummy)
		in
		Array.iteri processQuad block
	in
	Array.iteri processBlock blocks


let deadCodeElimination (blocks, succs, preds) =
	let (duChains, udChains) = ConstructDuUdChains.globalConstructDuUdChains (blocks, succs, preds) in
	let mark = Array.make (Array.length blocks) (Array.make 0 false) in
	let initWorkList = markEssential blocks mark in
	deadCodeElimination_aux blocks duChains udChains mark initWorkList;
	deleteUnmarkedQuads blocks mark



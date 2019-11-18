open Structs
open Quads
open Symbol
open Error
open ControlFlowGraph
open IterativeEquationsSolver


let insertAP (set, map) (op, l, block_nr, line) =
	match (op, l) with
	(* copy propagation *)
	| (OP_left (Left_entry e1), Left_entry e2) ->
		let key = (Some e1.entry_number, e2.entry_number, block_nr, line) in
		let newSet = QuadrupleSet.add key set in
		let newMap = QuadrupleMap.add key op map in
		(newSet, newMap)
	(* constant propagation *)
	| (OP_constant _, Left_entry e2) ->
		let key = (None, e2.entry_number, block_nr, line) in
		let newSet = QuadrupleSet.add key set in
		let newMap = QuadrupleMap.add key op map in
		(newSet, newMap)
	| _ ->
		(set, map)


let removeAP (set, map) l =
	match l with
	| Left_entry e ->
		let contains_e (a, b, _, _) = (e.entry_number = b) || ((Some e.entry_number) = a) in
		let subSet = QuadrupleSet.filter contains_e set in
		let newSet = QuadrupleSet.diff set subSet in
		let newMap = QuadrupleSet.fold (fun k m -> QuadrupleMap.remove k m) subSet map in
		(newSet, newMap)
	| _ ->
		(set, map)


let localPropagation block block_nr ap =
	let copyValue op (set, map) =
		match op with
		| OP_left (Left_entry e) -> begin
			let ap_e = QuadrupleSet.filter (fun (_, b, _, _) -> b = e.entry_number) set in
			match (QuadrupleSet.cardinal ap_e) with
			| 0 ->
				op
			| 1 ->
				let key = QuadrupleSet.choose ap_e in
				let op_a = QuadrupleMap.find key map in
				op_a
			| _ ->
				internal "Available propagations for one variable should be 0 or 1";
				raise Exit
		  end
		| OP_left (Left_deref e) -> begin
			let ap_e = QuadrupleSet.filter (fun (_, b, _, _) -> b = e.entry_number) set in
			match (QuadrupleSet.cardinal ap_e) with
			| 0 ->
				op
			| 1 -> begin
				let key = QuadrupleSet.choose ap_e in
				let op_a = QuadrupleMap.find key map in
				match op_a with
				| OP_left (Left_entry a) ->
					OP_left (Left_deref a)
				| _ ->
					internal "Available propagation for a variable of type reference\n \
						should be another variable of the same type";
					raise Exit
			  end
			| _ ->
				internal "Available propagations for one variable should be 0 or 1";
				raise Exit
		  end
		| _ ->
			op
	in
	let copyLeftValue l (set, map) =
		match l with
		| Left_deref e -> begin
			let ap_e = QuadrupleSet.filter (fun (_, b, _, _) -> b = e.entry_number) set in
			match (QuadrupleSet.cardinal ap_e) with
			| 0 ->
				l
			| 1 -> begin
				let key = QuadrupleSet.choose ap_e in
				let op_a = QuadrupleMap.find key map in
				match op_a with
				| OP_left (Left_entry a) ->
					Left_deref a
				| _ ->
					internal "Available propagation for a variable of type reference\n \
						should be another variable of the same type";
					raise Exit
			  end
			| _ ->
				internal "Available propagations for one variable should be 0 or 1";
				raise Exit
		  end
		| _ ->
			l
	in
	let propagate (i, ap) (qLabel, quad) =
		match quad with
		(* copy, remove, insert *)
		| Q_assign (op, l) ->
			let newOP = copyValue op ap in
			let newL = copyLeftValue l ap in
			block.(i) <- (qLabel, Q_assign (newOP, newL));
			let newAP = removeAP ap l in
			(i + 1, insertAP newAP (newOP, newL, block_nr, i))
		(* copy, remove *)
		| Q_calculate (op1, oper, op2, l) ->
			let newOP1 = copyValue op1 ap in
			let newOP2 = copyValue op2 ap in
			let newL = copyLeftValue l ap in
			block.(i) <- (qLabel, Q_calculate (newOP1, oper, newOP2, newL));
			(i + 1, removeAP ap l)
		| Q_array (e, op, l) ->
			let newOP = copyValue op ap in
			let newL = copyLeftValue l ap in
			block.(i) <- (qLabel, Q_array (e, newOP, newL));
			(i + 1, removeAP ap l)
		| Q_dim (e, d, l) ->
			let newL = copyLeftValue l ap in
			block.(i) <- (qLabel, Q_dim (e, d, newL));
			(i + 1, removeAP ap l)
		| Q_call (l, parl, Some r) ->
			let newL = copyLeftValue l ap in
			let newParl = List.map (fun (Par p) -> Par (copyValue p ap)) parl in
			let newR = Some (copyLeftValue r ap) in
			block.(i) <- (qLabel, Q_call (newL, newParl, newR));
			(i + 1, removeAP ap r)
		(* copy *)
		| Q_ifb (op, t) ->
			let newOP = copyValue op ap in
			block.(i) <- (qLabel, Q_ifb (newOP, t));
			(i + 1, ap)
		| Q_compare (op1, rel, op2, t) ->
			let newOP1 = copyValue op1 ap in
			let newOP2 = copyValue op2 ap in
			block.(i) <- (qLabel, Q_compare (newOP1, rel, newOP2, t));
			(i + 1, ap)
		| Q_call (l, parl, None) ->
			let newL = copyLeftValue l ap in
			let newParl = List.map (fun (Par p) -> Par (copyValue p ap)) parl in
			block.(i) <- (qLabel, Q_call (newL, newParl, None));
			(i + 1, ap)
		| Q_tailRecCall (e, parl) ->
			let newParl = List.map (fun (Par p) -> Par (copyValue p ap)) parl in
			block.(i) <- (qLabel, Q_tailRecCall (e, newParl));
			(i + 1, ap)
		| Q_return op ->
			let newOP = copyValue op ap in
			block.(i) <- (qLabel, Q_return newOP);
			(i + 1, ap)
		| _ ->
			(i + 1, ap)
	in
	ignore (Array.fold_left propagate (0, ap) block)


let globalPropagation (blocks, succs, preds) =
	let gen = Array.make (Array.length blocks) (QuadrupleSet.empty, QuadrupleMap.empty) in
	let prsv = Array.make (Array.length blocks) (QuadrupleSet.empty, QuadrupleMap.empty) in
	let fill_gen i block =
		let insert_gen j (_, quad) =
			match quad with
			| Q_assign (op, l) ->
				gen.(i) <- removeAP gen.(i) l;
				gen.(i) <- insertAP gen.(i) (op, l, i, j)
			| Q_calculate (_, _, _, l)
			| Q_array (_, _, l)
			| Q_dim (_, _, l)
			| Q_call (_, _, Some l) ->
				gen.(i) <- removeAP gen.(i) l
			| _ ->
				()
		in
		Array.iteri insert_gen block
	in
	Array.iteri fill_gen blocks;
	let fill_universal (uset, umap) (iset, imap) =
		let f k a b =
			match (a, b) with
			| (Some x, None) ->
				Some x
			| (None, Some y) ->
				Some y
			(* to kathe gen.(i) exei tous orsimous tou blocks.(i) opote
			   den prokeite to umap kai to imap na exoun koino kleidi *)
			| _ ->
				None
		in
		(QuadrupleSet.union uset iset, QuadrupleMap.merge f umap imap)
	in
	let universal = Array.fold_left fill_universal (QuadrupleSet.empty, QuadrupleMap.empty) gen in
	let fill_prsv i block =
		let remove_prsv (_, quad) =
			match quad with
			| Q_assign (_, l)
			| Q_calculate (_, _, _, l)
			| Q_array (_, _, l)
			| Q_dim (_, _, l)
			| Q_call (_, _, Some l) ->
				prsv.(i) <- removeAP prsv.(i) l
			| _ ->
				()
		in
		prsv.(i) <- universal;
		Array.iter remove_prsv block
	in
	Array.iteri fill_prsv blocks;
	let n = Array.length blocks in
	let r = 0 in
	let (_, _, post) = ControlFlowGraph.dfsPP n succs r in
	let apin = Array.make n QuadrupleSet.empty in
	let init = QuadrupleSet.empty in
	let f p set = QuadrupleSet.union (fst gen.(p)) (QuadrupleSet.inter set (fst prsv.(p))) in
	let pathCombiningFunction = QuadrupleSet.inter in
	let neutral = fst universal in
	let equalSets = QuadrupleSet.equal in
	IterativeEquationsSolver.worklist_iterate
		post r f succs preds apin init (neutral, pathCombiningFunction) equalSets;
	let umap = snd universal in
	let ap = Array.make (Array.length blocks) (QuadrupleSet.empty, QuadrupleMap.empty) in
	let process i apin_i =
		let map_i = QuadrupleMap.filter (fun k v -> QuadrupleSet.mem k apin_i) umap in
		ap.(i) <- (apin_i, map_i)
	in
	Array.iteri process apin;
	Array.iteri (fun i ap_i -> localPropagation blocks.(i) i ap_i) ap


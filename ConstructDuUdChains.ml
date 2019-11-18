open Structs
open Quads
open Symbol
open ReachingDefinitions


let localConstructDuUdChains block block_nr defs duChain udChain =
	let insert set (l, block_nr, line) =
		match l with
		| Left_entry e ->
			TripleSet.add (e.entry_number, block_nr, line) set
		| _ ->
			set
	in
	let remove set l =
		match l with
		| Left_entry e ->
			TripleSet.filter (fun (a, _, _) -> a <> e.entry_number) set
		| _ ->
			set
	in
	let insertDuUdChains op i j set duCh udCh =
		match op with
		| OP_left (Left_entry e)
		| OP_left (Left_deref e) ->
			let defs_e = TripleSet.filter (fun (a, _, _) -> a = e.entry_number) set in
			let process (a, b, l) (duCh1, udCh1) =
				let duCh1_e_b_l =
					try TripleMap.find (a, b, l) duCh1 with Not_found -> []
				in
				let duCh2 = TripleMap.add (a, b, l) ((i, j) :: duCh1_e_b_l) duCh1 in
				let udCh1_e_i_j =
					try TripleMap.find (a, i, j) udCh1 with Not_found -> []
				in
				let udCh2 = TripleMap.add (a, i, j) ((b, l) :: udCh1_e_i_j) udCh1 in
				(duCh2, udCh2)
			in
			TripleSet.fold process defs_e (duCh, udCh)
		| _ ->
			(duCh, udCh)
	in
	let insertLvalueDuUdChains l i j set duCh udCh =
		match l with
		| Left_deref e ->
			insertDuUdChains (OP_left l) i j set duCh udCh
		| _ ->
			(duCh, udCh)
	in
	let process (i, set, duCh, udCh) (_, quad) =
		let addPar (duChain, udChain) (Par p) =
			insertDuUdChains p block_nr i set duChain udChain
		in
		match quad with
		| Q_assign (op, l) ->
			let (duCh1, udCh1) = insertDuUdChains op block_nr i set duCh udCh in
			let (duCh2, udCh2) = insertLvalueDuUdChains l block_nr i set duCh1 udCh1 in
			let newSet = remove set l in
			(i + 1, insert newSet (l, block_nr, i), duCh2, udCh2)
		| Q_calculate (op1, _, op2, l) ->
			let (duCh1, udCh1) = insertDuUdChains op1 block_nr i set duCh udCh in
			let (duCh2, udCh2) = insertDuUdChains op2 block_nr i set duCh1 udCh1 in
			let (duCh3, udCh3) = insertLvalueDuUdChains l block_nr i set duCh2 udCh2 in
			let newSet = remove set l in
			(i + 1, insert newSet (l, block_nr, i), duCh3, udCh3)
		| Q_array (e, op, l) ->
			let (duCh1, udCh1) = insertDuUdChains (OP_left (Left_entry e)) block_nr i set duCh udCh in
			let (duCh2, udCh2) = insertDuUdChains op block_nr i set duCh1 udCh1 in
			let (duCh3, udCh3) = insertLvalueDuUdChains l block_nr i set duCh2 udCh2 in
			let newSet = remove set l in
			(i + 1, insert newSet (l, block_nr, i), duCh3, udCh3)
		| Q_dim (e, _, l) ->
			let (duCh1, udCh1) = insertDuUdChains (OP_left (Left_entry e)) block_nr i set duCh udCh in
			let (duCh2, udCh2) = insertLvalueDuUdChains l block_nr i set duCh1 udCh1 in
			let newSet = remove set l in
			(i + 1, insert newSet (l, block_nr, i), duCh2, udCh2)
		| Q_call (l, parl, Some r) ->
			let (duCh1, udCh1) = List.fold_left addPar (duCh, udCh) parl in
			let (duCh2, udCh2) = insertDuUdChains (OP_left l) block_nr i set duCh1 udCh1 in
			let (duCh3, udCh3) = insertLvalueDuUdChains r block_nr i set duCh2 udCh2 in
			let newSet = remove set r in
			(i + 1, insert newSet (r, block_nr, i), duCh3, udCh3)
		| Q_call (l, parl, None) ->
			let (duCh1, udCh1) = List.fold_left addPar (duCh, udCh) parl in
			let (duCh2, udCh2) = insertDuUdChains (OP_left l) block_nr i set duCh1 udCh1 in
			(i + 1, set, duCh2, udCh2)
		| Q_tailRecCall (e, parl) ->
			let (duCh1, udCh1) = List.fold_left addPar (duCh, udCh) parl in
			let (duCh2, udCh2) = insertDuUdChains (OP_left (Left_entry e)) block_nr i set duCh1 udCh1 in
			(i + 1, set, duCh2, udCh2)
		| Q_return op
		| Q_ifb (op, _) ->
			let (duCh1, udCh1) = insertDuUdChains op block_nr i set duCh udCh in
			(i + 1, set, duCh1, udCh1)
		| Q_compare (op1, _, op2, _) ->
			let (duCh1, udCh1) = insertDuUdChains op1 block_nr i set duCh udCh in
			let (duCh2, udCh2) = insertDuUdChains op2 block_nr i set duCh1 udCh1 in
			(i + 1, set, duCh2, udCh2)
		| _ ->
			(i + 1, set, duCh, udCh)
	in
	let (_, _, newDuChain, newUdChain) = Array.fold_left process (0, defs, duChain, udChain) block in
	(newDuChain, newUdChain)


let globalConstructDuUdChains (blocks, succs, preds) =
	let rchin = ReachingDefinitions.reachingDefinitions (blocks, succs, preds) in
	let process (i, duCh, udCh) block =
		let (newDuCh, newUdCh) = localConstructDuUdChains block i rchin.(i) duCh udCh in
		(i + 1, newDuCh, newUdCh)
	in
	let (_, finalDuChain, finalUdChain) = Array.fold_left process (0, TripleMap.empty, TripleMap.empty) blocks in
	(finalDuChain, finalUdChain)




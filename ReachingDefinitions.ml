open Structs
open Quads
open IterativeEquationsSolver
open Symbol


let reachingDefinitions (blocks, succs, preds) =
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
	let prsv = Array.make (Array.length blocks) TripleSet.empty in
	let gen = Array.make (Array.length blocks) TripleSet.empty in
	let fill_gen i block =
		let insert_gen j (_, quad) =
			match quad with
			| Q_assign (_, l)
			| Q_calculate (_, _, _, l)
			| Q_array (_, _, l)
			| Q_dim (_, _, l)
			| Q_call (_, _, Some l) ->
				gen.(i) <- remove gen.(i) l;
				gen.(i) <- insert gen.(i) (l, i, j)
			| _ ->
				()
		in
		Array.iteri insert_gen block
	in
	Array.iteri fill_gen blocks;
	let universal = Array.fold_left (fun uset iset -> TripleSet.union uset iset) TripleSet.empty gen in
	let fill_prsv i block =
		let remove_prsv (_, quad) =
			match quad with
			| Q_assign (_, l)
			| Q_calculate (_, _, _, l)
			| Q_array (_, _, l)
			| Q_dim (_, _, l)
			| Q_call (_, _, Some l) ->
				prsv.(i) <- remove prsv.(i) l
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
	let rchin = Array.make n TripleSet.empty in
	let init = TripleSet.empty in
	let f p set = TripleSet.union gen.(p) (TripleSet.inter set prsv.(p)) in
	let pathCombiningFunction = TripleSet.union in
	let neutral = TripleSet.empty in
	let equalSets = TripleSet.equal in
	IterativeEquationsSolver.worklist_iterate 
		post r f succs preds rchin init (neutral, pathCombiningFunction) equalSets;
	rchin
	



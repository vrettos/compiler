open Quads
open Structs
open Error
open ControlFlowGraph


let branchSimplifications (blocks, succs, preds) funcBaseLabel =
	let getBlockBranches (i, initMap) block =
		let getBranch (j, map) (qLabel, quad) =
			match quad with
			| _ when (quad = quad_dummy) ->
				(j + 1, map)
			| Q_jump _
			| Q_ifb _
			| Q_compare _ ->
				(j + 1, IntMap.add qLabel (i, j) map)
			| _ ->
				(j + 1, map)
		in
		let resMap = snd (Array.fold_left getBranch (0, initMap) block) in
		(i + 1, resMap)
	in
	let branchesMap = snd (Array.fold_left getBlockBranches (0, IntMap.empty) blocks) in
	let simplify qLabel (i, j) changed =
		let quad = snd blocks.(i).(j) in
		match quad with
		| Q_jump t -> begin
			try (
				let (i_t, j_t) = IntMap.find t branchesMap in
				let quad_t = snd blocks.(i_t).(j_t) in
				match quad_t with
				| Q_jump l when (t <> l) ->
					blocks.(i).(j) <- (qLabel, quad_t);
					true
				| _ ->
					changed
			) with Not_found ->
				changed
		  end
		| Q_ifb (op, t) -> begin
			try (
				let (i_t, j_t) = IntMap.find t branchesMap in
				let quad_t = snd blocks.(i_t).(j_t) in
				match quad_t with
				| Q_jump l when (t <> l) ->
					blocks.(i).(j) <- (qLabel, Q_ifb (op, l));
					true
				| _ ->
					changed
			) with Not_found ->
				changed
		  end
		| Q_compare (op1, rel, op2, t) -> begin
			try (
				let (i_t, j_t) = IntMap.find t branchesMap in
				let quad_t = snd blocks.(i_t).(j_t) in
				match quad_t with
				| Q_jump l when (t <> l) ->
					blocks.(i).(j) <- (qLabel, Q_compare (op1, rel, op2, l));
					true
				| _ ->
					changed
			) with Not_found ->
				changed
		  end
		| _ ->
			internal "It should be a branch";
			raise Exit
	in
	while (IntMap.fold simplify branchesMap false) do () done;
	ControlFlowGraph.updateControlFlowGraph funcBaseLabel (blocks, succs, preds)	


let removeNextQuadBranches (blocks, succs, preds) =
	let processBlock block =
		let processQuad j (qLabel, quad) =
			match quad with
			| _ when (quad = quad_dummy) ->
				()
			| Q_jump t
			| Q_ifb (_, t)
			| Q_compare (_, _, _, t) when (t = qLabel + 1) ->
				block.(j) <- (qLabel, quad_dummy)
			| _ ->
				()
		in
		Array.iteri processQuad block
	in
	Array.iter processBlock blocks



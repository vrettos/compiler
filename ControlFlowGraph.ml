open Structs
open Quads
open Error


let controlFlowGraph_of_function functionQuads =
	let find_leaders quadsList =
		let process (branch, leaders) (qLab, quad) =
			let newLeaders =
				if (branch) then IntSet.add qLab leaders
				else leaders
			in
			match quad with
			| _ when (quad = quad_dummy) ->
				(false, newLeaders)
			| Q_jump t
			| Q_ifb (_, t)
			| Q_compare (_, _, _, t) ->
				(true, IntSet.add t newLeaders)
			| Q_tailRecCall _ ->
				(true, newLeaders)
			| _ ->
				(false, newLeaders)
		in
		IntSet.elements (snd (List.fold_left process (true, IntSet.empty) quadsList))
	in
	let leaders = find_leaders functionQuads in
	let graphSize = List.length leaders in
	let blocks = Array.create graphSize (Array.create 0 (star, quad_dummy)) in
	let succs = Array.create graphSize [] in
	let preds = Array.create graphSize [] in
	let funcQdsArray = Array.of_list functionQuads in
	let firstLeader = fst funcQdsArray.(0) in
	let lastLeader = (fst funcQdsArray.((Array.length funcQdsArray) - 1)) + 1 in
	let nextLeaders = (List.tl leaders) @ [lastLeader] in
	let fill_block i (leader, nextLeader) =
		blocks.(i) <- Array.init (nextLeader - leader) (fun j -> funcQdsArray.(leader - firstLeader + j));
		i + 1
	in
	ignore (List.fold_left fill_block 0 (List.combine leaders nextLeaders));
	let expandMapping (i, map) leader = (i + 1, IntMap.add leader i map) in
	let block_of_leader = snd (List.fold_left expandMapping (0, IntMap.empty) leaders) in
	let lastBlock = graphSize - 1 in
	let fill_edges i block =
		if (i <> lastBlock) then (
			let blockLastQuad = snd (block.((Array.length block) - 1)) in
			match blockLastQuad with
			| _ when (blockLastQuad = quad_dummy) ->
				succs.(i) <- (i + 1) :: succs.(i);
				preds.(i + 1) <- i :: preds.(i + 1)
			| Q_jump t ->
				let j = IntMap.find t block_of_leader in
				succs.(i) <- j :: succs.(i);
				preds.(j) <- i :: preds.(j)
			| Q_ifb (_, t)
			| Q_compare (_, _, _, t) ->
				let j = IntMap.find t block_of_leader in
				succs.(i) <- j :: succs.(i);
				preds.(j) <- i :: preds.(j);
				if (j <> i + 1) then (
					succs.(i) <- (i + 1) :: succs.(i);
					preds.(i + 1) <- i :: preds.(i + 1)
				)
			| Q_tailRecCall _ ->
				()
			| _ ->
				succs.(i) <- (i + 1) :: succs.(i);
				preds.(i + 1) <- i :: preds.(i + 1)
		)
	in
	Array.iteri fill_edges blocks;
	(blocks, succs, preds)


let function_of_controlFlowGraph funcBaseLabel (blocks, succs, preds) =
	let reorderBlockLabels (initLabel, initMap, initDummies) block =
		let reorderLabel (newLabel, labelMap, dummies, i) (exLabel, quad) =
			match quad with
			| _ when (quad = quad_dummy) ->
				(newLabel, labelMap, exLabel :: dummies, i + 1)
			| _ ->
				let process map t =
					IntMap.add t newLabel map
				in
				let newLabelMap = List.fold_left process labelMap (exLabel :: dummies) in
				block.(i) <- (newLabel, quad);
				(newLabel + 1, newLabelMap, [], i + 1)
		in
		let (resLabel, resMap, resDummies, _) = 
			Array.fold_left reorderLabel (initLabel, initMap, initDummies, 0) block
		in
		(resLabel, resMap, resDummies)
	in
	let (nextFuncBaseLabel, labelMap, _) = 
		Array.fold_left reorderBlockLabels (funcBaseLabel, IntMap.empty, []) blocks 
	in
	let fixBlockBranches_filterDummies acc block =
		let fixBranch_filterDummy acc (label, quad) =
			match quad with
			| _ when (quad = quad_dummy) ->
				acc
			| Q_jump t -> begin
				try (
					let newT = IntMap.find t labelMap in
					(label, Q_jump newT) :: acc
				) with Not_found ->
					internal "Jump to invalid label";
					raise Exit
			  end
			| Q_ifb (op, t) -> begin
				try (
					let newT = IntMap.find t labelMap in
					(label, Q_ifb (op, newT)) :: acc
				) with Not_found ->
					internal "Jump to invalid label";
					raise Exit
			  end
			| Q_compare (op1, rel, op2, t) -> begin
				try (
					let newT = IntMap.find t labelMap in
					(label, Q_compare (op1, rel, op2, newT)) :: acc
				) with Not_found ->
					internal "Jump to invalid label";
					raise Exit
			  end
			| _ ->
				(label, quad) :: acc
		in
		Array.fold_left fixBranch_filterDummy acc block 
	in
	(nextFuncBaseLabel, List.rev (Array.fold_left fixBlockBranches_filterDummies [] blocks))


let updateControlFlowGraph funcBaseLabel (blocks, succs, preds) =
	let (_, funcQds) = function_of_controlFlowGraph funcBaseLabel (blocks, succs, preds) in
	controlFlowGraph_of_function funcQds


let rec dfsPP_aux succs x visit pre post i j =
	visit.(x) <- true;
	pre.(x) <- !j;
	incr j;
	let process y =
		if (not visit.(y)) then dfsPP_aux succs y visit pre post i j
		else ()
	in
	List.iter process succs.(x);
	post.(x) <- !i;
	incr i


let dfsPP n succs r =
	let visit = Array.make n false in
	let pre = Array.make n 0 in
	let post = Array.make n 0 in
	let i = ref 1 in
	let j = ref 1 in
	dfsPP_aux succs r visit pre post i j;
	(visit, pre, post)



open Structs


let rec worklist_iterate_aux order worklist f succs preds dfin (neutral, pathCombiningFunction) equalSets =
	if (not (IntMap.is_empty worklist)) then (
		let (k, b) = IntMap.max_binding worklist in
		let newWorklist = IntMap.remove k worklist in
		let process effect p = pathCombiningFunction effect (f p dfin.(p)) in
		let totaleffect = List.fold_left process neutral preds.(b) in
		if (not (equalSets dfin.(b) totaleffect)) then (
			dfin.(b) <- totaleffect;
			let expand wlst bi = IntMap.add order.(bi) bi wlst in
			let expandedWorklist = List.fold_left expand newWorklist succs.(b) in
			worklist_iterate_aux
				order expandedWorklist f succs preds dfin (neutral, pathCombiningFunction) equalSets
		)
		else (
			worklist_iterate_aux
				order newWorklist f succs preds dfin (neutral, pathCombiningFunction) equalSets
		)
	)


let worklist_iterate order entry f succs preds dfin init (neutral, pathCombiningFunction) equalSets =
	Array.iteri (fun bi _ -> dfin.(bi) <- neutral) dfin;
	dfin.(entry) <- init;
	let process (bi, wlst) orderBi =
		if (bi <> entry) then
			(bi + 1, IntMap.add orderBi bi wlst)
		else
			(bi + 1, wlst)
	in	
	let worklist = snd (Array.fold_left process (0, IntMap.empty) order) in
	worklist_iterate_aux order worklist f succs preds dfin (neutral, pathCombiningFunction) equalSets


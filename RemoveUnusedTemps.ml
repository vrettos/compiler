open Symbol
open Quads
open Error
open Structs


let removeUnusedTemps funcQds =
	let (outer_func, outer_func_info) = 
		match (snd (List.hd funcQds)) with
		| Q_unit e -> begin
			match e.entry_info with
			| ENTRY_function inf ->
				(e, inf)
			| _ ->
				internal "It should be a function entry";
				raise Exit
		  end
		| _ ->
			internal "It should be a new unit quad";
			raise Exit
	in
	let findUsedTemps map (_, quad) =
		let insertLeft l m =
			match l with
			| Left_entry e
			| Left_deref e -> begin
				match e.entry_info with
				| ENTRY_temporary _ ->
					IntMap.add e.entry_number e m
				| _ ->
					m
			  end				
		in
		let insertOP op m =
			match op with
			| OP_left l ->
				insertLeft l m
			| OP_constant _ ->
				m
		in
		match quad with
		| Q_ifb (op, _)
		| Q_return op ->
			insertOP op map
		| Q_compare (op1, _, op2, _) ->
			let map1 = insertOP op1 map in
			insertOP op2 map1
		| Q_calculate (op1, _, op2, l) ->
			let map1 = insertOP op1 map in
			let map2 = insertOP op2 map1 in
			insertLeft l map2
		| Q_array (_, op, l)
		| Q_assign (op, l) ->
			let map1 = insertOP op map in
			insertLeft l  map1
		| Q_dim (_, _, l) ->
			insertLeft l map
		| Q_call (l, parl, None) ->
			let map1 = List.fold_left (fun m (Par p) -> insertOP p m) map parl in
			insertLeft l map1
		| Q_call (l, parl, Some r) ->
			let map1 = List.fold_left (fun m (Par p) -> insertOP p m) map parl in
			let map2 = insertLeft l map1 in
			insertLeft r map2
		| Q_tailRecCall (_, parl) ->
			List.fold_left (fun m (Par p) -> insertOP p m) map parl
		| _ ->
			map
	in
	let usedTempsMap = List.fold_left findUsedTemps IntMap.empty funcQds in
	let usedTempsList = IntMap.fold (fun _ e acc -> e :: acc) usedTempsMap [] in
	outer_func_info.function_templist <- usedTempsList;
	newFunctionFixOffsets outer_func




open ControlFlowGraph
open Quads


let unreachableCodeElimination (blocks, succs, preds) =
	let n = Array.length blocks in
	let (visit, _, _) = dfsPP n succs 0 in
	let remove i visit_i =
		if (not visit_i) then (
			Array.iteri (fun j (qLabel, _) -> blocks.(i).(j) <- (qLabel, quad_dummy)) blocks.(i)
		)
	in
	Array.iteri remove visit



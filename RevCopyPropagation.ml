open Quads


let revCopyPropagation (blocks, succs, preds) =
	let propagateBlock i block =
		for j = 0 to ((Array.length block) - 2) do
			let qLabel1 = fst block.(j) in
			let qLabel2 = fst block.(j + 1) in
			match (snd block.(j), snd block.(j + 1)) with
			| (Q_calculate (op1, oper, op2, l11), Q_assign ((OP_left l12), l2)) when (equalLeft l11 l12)->
				block.(j) <- (qLabel1, Q_calculate (op1, oper, op2, l2));
				block.(j + 1) <- (qLabel2, Q_assign ((OP_left l2), l11))
			| (Q_array (e, op, l11), Q_assign ((OP_left l12), l2)) when (equalLeft l11 l12) ->
				block.(j) <- (qLabel1, Q_array (e, op, l2));
				block.(j + 1) <- (qLabel2, Q_assign ((OP_left l2), l11))
			| (Q_dim (e, d, l11), Q_assign ((OP_left l12), l2)) when (equalLeft l11 l12) ->
				block.(j) <- (qLabel1, Q_dim (e, d, l2));
				block.(j + 1) <- (qLabel2, Q_assign ((OP_left l2), l11))
			| (Q_call (l, parl, Some l11), Q_assign ((OP_left l12), l2)) when (equalLeft l11 l12) ->
				block.(j) <- (qLabel1, Q_call (l, parl, Some l2));
				block.(j + 1) <- (qLabel2, Q_assign ((OP_left l2), l11))
			| _ ->
				()
		done
	in
	Array.iteri propagateBlock blocks



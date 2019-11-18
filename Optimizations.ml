open ControlFlowGraph
open Propagation
open ConstantFolding
open DeadCodeElimination
open UnreachableCodeElimination
open BranchSimplification
open RemoveUnusedTemps
open RevCopyPropagation


let optimize quadListList =
	let optimize_function (funcBaseLabel, acc) funcQuads =
		let controlFlowGraph1 = ControlFlowGraph.controlFlowGraph_of_function funcQuads in
		RevCopyPropagation.revCopyPropagation controlFlowGraph1;
		Propagation.globalPropagation controlFlowGraph1;
		while (ConstantFolding.constantFolding controlFlowGraph1) do
			Propagation.globalPropagation controlFlowGraph1
		done;
		DeadCodeElimination.deadCodeElimination controlFlowGraph1;
		let controlFlowGraph2 = BranchSimplification.branchSimplifications controlFlowGraph1 funcBaseLabel in
		UnreachableCodeElimination.unreachableCodeElimination controlFlowGraph2;
		let controlFlowGraph3 = ControlFlowGraph.updateControlFlowGraph funcBaseLabel controlFlowGraph2 in
		BranchSimplification.removeNextQuadBranches controlFlowGraph3;
		let (nextFuncBaseLabel, funcQuads1) = 
			ControlFlowGraph.function_of_controlFlowGraph funcBaseLabel controlFlowGraph3
		in
		RemoveUnusedTemps.removeUnusedTemps funcQuads1;
		(nextFuncBaseLabel, funcQuads1 :: acc)
	in
	let (_, revOptimizedQuadListList) = List.fold_left optimize_function (1, []) quadListList in
	List.rev revOptimizedQuadListList




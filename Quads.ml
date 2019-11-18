open Types
open Symbol
open Error
open Structs
open Print

type quad =
    Q_unit of Symbol.entry
  | Q_endu of Symbol.entry
  | Q_jump of int
  | Q_ifb of operand * int
  | Q_compare of operand * relation * operand * int
  | Q_calculate of operand * operator * operand * left
  | Q_assign of operand * left
  | Q_array of Symbol.entry * operand * left
  | Q_dim of Symbol.entry * int * left
  | Q_call of left * parameter list * result
  | Q_tailRecCall of Symbol.entry * parameter list
  | Q_return of operand
and operand = OP_constant of constant | OP_left of left
and constant =
    Const_int of int
  | Const_float of float
  | Const_bool of bool
  | Const_char of char
  | Const_string of string
and left = Left_entry of Symbol.entry | Left_deref of Symbol.entry
and parameter = Par of operand
and result = left option
and relation = 
    EQ
  | NEQ
  | GE
  | LE
  | GEQ
  | LEQ
  | FEQ
  | FNEQ
  | FGE
  | FLE
  | FGEQ
  | FLEQ
  | CEQ
  | CNEQ
  | CGE
  | CLE
  | CGEQ
  | CLEQ
and operator = ADD | SUB | MUL | DIV | MOD | FADD | FSUB | FMUL | FDIV
and place =
    Place of operand
  | TFLists of quad ref list * quad ref list
  | Unit
  | TailRecCall


let getLeftTyp l =
	match l with
	| Left_entry e ->
		getEntryTyp e
	| Left_deref e -> begin
		let typ = getEntryTyp e in
		match typ with
		| TYPE_ref tau ->
			tau
		| _ ->
			internal "It should be a reference";
			raise Exit
	  end


let getOPTyp op =
	match op with
	| OP_left l ->
		getLeftTyp l
	| OP_constant (Const_int _) ->
		TYPE_int
	| OP_constant (Const_char _) ->
		TYPE_char
	| OP_constant (Const_bool _) ->
		TYPE_bool
	| OP_constant (Const_float _) ->
		TYPE_float
	| OP_constant (Const_string s) ->
		TYPE_array (TYPE_char, String.length s)


let equalLeft l1 l2 =
	match (l1, l2) with
	| (Left_entry e1, Left_entry e2)
	| (Left_deref e1, Left_deref e2) ->
		e1.entry_number = e2.entry_number
	| _ ->
		false


let star = 0


let quad_dummy = Q_jump (-42)


let nextQuad = ref 1


let readyFunctions = ref []


let underConstructionFunctions = ref []


let genQuadRet quad =
	let quadRef = ref quad in
	let labelRef = ref !nextQuad in
	incr nextQuad;
	let qd = (labelRef, quadRef) in
	match quad with
	| Q_unit _ ->
		underConstructionFunctions := [qd] :: !underConstructionFunctions;
		quadRef
	| Q_endu _ ->
		let rdFunc = qd :: (List.hd !underConstructionFunctions) in
		readyFunctions := rdFunc :: !readyFunctions;
		underConstructionFunctions := List.tl !underConstructionFunctions;
		quadRef
	| _ ->
		underConstructionFunctions :=
			(qd :: (List.hd !underConstructionFunctions))
			:: (List.tl !underConstructionFunctions);
		quadRef


let genQuad quad = ignore (genQuadRet quad)


let patch quadRefList z =
	let fixLabel quadRef =
		match !quadRef with
		| Q_jump _ ->
			quadRef := Q_jump z
		| Q_ifb (p, _) ->
			quadRef := Q_ifb (p, z)
		| Q_compare (p1, rel, p2, _) ->
			quadRef := Q_compare (p1, rel, p2, z)
		| _ ->
			internal "Non patchable quad";
			raise Exit
	in
	List.iter fixLabel quadRefList


let reorderLabels lR_X_qRListList =
	let reorderFunctionsLabels lR_X_qRList (initLabel, initMap) =
		let reorderQuadsLabel (lRef, qRef) (newLabel, labelMap, dummies) =
			match !qRef with
			| _ when (!qRef = quad_dummy) ->
				(newLabel, labelMap, !lRef :: dummies)
			| _ ->
				let process map exLabel =
					IntMap.add exLabel newLabel map
				in
				let newLabelMap = List.fold_left process labelMap (!lRef :: dummies) in
				lRef := newLabel;
				(newLabel + 1, newLabelMap, [])
		in
		let (resLabel, resMap, _) = List.fold_right reorderQuadsLabel lR_X_qRList (initLabel, initMap, []) in
		(resLabel, resMap)
	in
	snd (List.fold_right reorderFunctionsLabels lR_X_qRListList (1, IntMap.empty))


let fixBranches_RevLists_FilterDummy labelMap lR_X_qRListList =
	let fixFunctionsBranches acc lR_X_qRList =
		let fixQuadsBranch functionsAcc (lRef, qRef) =
			match !qRef with
			| _ when (!qRef = quad_dummy) ->
				functionsAcc
			| Q_jump l
			| Q_ifb (_, l)
			| Q_compare (_, _, _, l) when (l = star) ->
				internal "There exists jump *";
				raise Exit
			| Q_jump l-> begin
				try (
					let newL = IntMap.find l labelMap in
					(!lRef, Q_jump newL) :: functionsAcc
				) with Not_found ->
					internal "Jump to invalid label";
					raise Exit
			  end	
			| Q_ifb (p, l) -> begin
				try (
					let newL = IntMap.find l labelMap in
					(!lRef, Q_ifb (p, newL)) :: functionsAcc
				) with Not_found ->
					internal "Jump to invalid label";
					raise Exit
			  end
			| Q_compare (p1, rel, p2, l) -> begin
				try (
					let newL = IntMap.find l labelMap in
					(!lRef, Q_compare (p1, rel, p2, newL)) :: functionsAcc
				) with Not_found ->
					internal "Jump to invalid label";
					raise Exit
			  end
			| _ ->
				(!lRef, !qRef) :: functionsAcc
		in
		(List.fold_left fixQuadsBranch [] lR_X_qRList) :: acc
	in
	List.fold_left fixFunctionsBranches [] lR_X_qRListList


let finalizeQuads () =
	let labelMap = reorderLabels !readyFunctions in
	fixBranches_RevLists_FilterDummy labelMap !readyFunctions



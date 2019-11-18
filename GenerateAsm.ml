open Structs
open Quads
open Types
open LibraryFunctions
open Asm
open Symbol
open Error


let libFuncsWH = ref StringMap.empty


let strWH = ref StringMap.empty


let nextStr = ref 1


let realWH = ref FloatMap.empty


let nextReal = ref 1


let label_of_int t = "@" ^ (string_of_int t)


let name func_ent =
	let func_name = func_ent.entry_name in
	match func_ent.entry_info with
	| ENTRY_function inf when (inf.function_library)->
		let name = "_" ^ func_name in
		libFuncsWH := StringMap.add name [Inst (Extrn name)] !libFuncsWH;
		name
	| ENTRY_function _ when (func_name = "_outer") ->
		"__outer"
	| ENTRY_function _ ->
		"_" ^ func_name ^ "_" ^ (string_of_int func_ent.entry_number)
	| _ ->
		internal "It should be a function entry";
		raise Exit


let getAR e =
	Inst (Mov (Reg Si, Mem (VarPlusOffset (Word, "display", 2 * e.entry_nesting))))


let rec load reg op func_depth acc =
	let saveStr str =
		let explode s =
			let rec exp i l =
				if (i < 0) then l
				else exp (i - 1) (s.[i] :: l)
			in
			 exp (String.length s - 1) []
		in
		try (
			fst (StringMap.find str !strWH)
		) with Not_found ->
			let symbolic_name = ("@str" ^ (string_of_int !nextStr)) in
			let init = [Inst (String (symbolic_name, String.length str))] in
			let process acc c =
				(Inst (Db (Char.code c))) :: acc
			in
			let body = List.fold_left process init (explode str) in
			strWH := StringMap.add str (symbolic_name, (Inst (Db 0)) :: body) !strWH;
			incr nextStr;
			symbolic_name
	in
	match op with
	| OP_constant (Const_int d) ->
		Inst (Mov (Reg reg, Imm (Int d))) :: acc
	| OP_constant (Const_char c) ->
		Inst (Mov (Reg reg, Imm (Int (Char.code c)))) :: acc
	| OP_constant (Const_float _) ->
		internal "loadReal should be called instead of load";
		raise Exit
	| OP_constant (Const_bool true) ->
		Inst (Mov (Reg reg, Imm (Int 1))) :: acc
	| OP_constant (Const_bool false) ->
		Inst (Mov (Reg reg, Imm (Int 0))) :: acc
	| OP_constant (Const_string s) ->
		let symbolic_name = saveStr s in
		(Inst (Lea (Reg reg, Mem (VarPlusOffset (Byte, symbolic_name, 0))))) :: acc
	| OP_left (Left_entry e) -> begin
		let ptr_type = ptr_type_of_reg reg in
		match e.entry_info with
		| ENTRY_function _ ->
			(Inst (Lea (Reg reg, Mem (VarPlusOffset (ptr_type, name e, 0))))) :: acc
		| _ ->
			let offset = getEntryOffset e in
			let local = (e.entry_nesting = func_depth) in
			if (local) then (
				(Inst (Mov (Reg reg, Mem (RegPlusOffset (ptr_type, Bp, offset))))) :: acc
			)
			else (
				let inst1 = getAR e in
				(Inst (Mov (Reg reg, Mem (RegPlusOffset (ptr_type, Si, offset))))) ::
				inst1 :: acc
			)
	  end
	| OP_left (Left_deref e) ->
		let ptr_type = ptr_type_of_reg reg in
		let acc0 = load Di (OP_left (Left_entry e)) func_depth acc in
		(Inst (Mov (Reg reg, Mem (RegPlusOffset (ptr_type, Di, 0))))) :: acc0


let store reg l func_depth acc =
	match l with
	| Left_entry e ->
		let ptr_type = ptr_type_of_reg reg in
		let offset = getEntryOffset e in
		let local = (e.entry_nesting = func_depth) in
		if (local) then (
			(Inst (Mov (Mem (RegPlusOffset (ptr_type, Bp, offset)), Reg reg))) :: acc
		)
		else (
			let inst1 = getAR e in
			(Inst (Mov (Mem (RegPlusOffset (ptr_type, Si, offset)), Reg reg))) ::
			inst1 :: acc
		)
	| Left_deref e ->
		let ptr_type = ptr_type_of_reg reg in
		let acc0 = load Di (OP_left (Left_entry e)) func_depth acc in
		(Inst (Mov (Mem (RegPlusOffset (ptr_type, Di, 0)), Reg reg))) :: acc0
		

let loadReal op func_depth acc =
	let saveReal real =
		try (
			fst (FloatMap.find real !realWH)
		) with Not_found ->
			let symbolic_name = "@real" ^ (string_of_int !nextReal) in
			let body = [Inst (Real (symbolic_name, real))] in
			realWH := FloatMap.add real (symbolic_name, body) !realWH;
			incr nextReal;
			symbolic_name
	in
	match op with
	| OP_constant (Const_float f) ->
		let symbolic_name = saveReal f in
		(Inst (Fld (Mem (VarPlusOffset (Tbyte, symbolic_name, 0))))) :: acc
	| OP_constant _ ->
		internal "load should be called instead of loadReal";
		raise Exit
	| OP_left (Left_entry e) ->
		let offset = getEntryOffset e in
		let local = (e.entry_nesting = func_depth) in
		if (local) then (
			(Inst (Fld (Mem (RegPlusOffset (Tbyte, Bp, offset))))) :: acc
		)
		else (
			let inst1 = getAR e in
			(Inst (Fld (Mem (RegPlusOffset (Tbyte, Si, offset))))) :: inst1 :: acc
		)
	| OP_left (Left_deref e) ->
		let acc0 = load Di (OP_left (Left_entry e)) func_depth acc in
		(Inst (Fld (Mem (RegPlusOffset (Tbyte, Di, 0))))) :: acc0


let storeReal l func_depth acc =
	match l with
	| Left_entry e ->
		let offset = getEntryOffset e in
		let local = (e.entry_nesting = func_depth) in
		if (local) then (
			(Inst (Fstp (Mem (RegPlusOffset (Tbyte, Bp, offset))))) :: acc
		)
		else (
			let inst1 = getAR e in
			(Inst (Fstp (Mem (RegPlusOffset (Tbyte, Si, offset))))) :: inst1 :: acc
		)
	| Left_deref e ->
		let acc0 = load Di (OP_left (Left_entry e)) func_depth acc in
		(Inst (Fstp (Mem (RegPlusOffset (Tbyte, Di, 0))))) :: acc0


let loadAddr reg l func_depth acc =
	match l with
	| Left_entry e ->
		let typ = getEntryTyp e in
		let ptr_type = ptr_type_of_type typ in
		let offset = getEntryOffset e in
		let local = (e.entry_nesting = func_depth) in
		if (local) then (
			Inst (Lea (Reg reg, Mem (RegPlusOffset (ptr_type, Bp, offset)))) :: acc
		)
		else (
			let inst1 = getAR e in
			Inst (Lea (Reg reg, Mem (RegPlusOffset (ptr_type, Si, offset)))) ::
			inst1 :: acc
		)
	| Left_deref e ->
		load reg (OP_left (Left_entry e)) func_depth acc


let asm_of_quad outer_func acc (qdLabel, quad) =
	let func_depth = outer_func.entry_nesting in
	let asm_of_par acc (Par op) =
		let typ = getOPTyp op in
		let ptr_type = ptr_type_of_type typ in
		match ptr_type with
		| Byte ->
			let acc0 = load Al op func_depth acc in
			let inst1 = Inst (Sub (Reg Sp, Imm (Int sizeOfType typ))) in
			let inst2 = Inst (Mov (Reg Si, Reg Sp)) in
			let inst3 = Inst (Mov (Mem (RegPlusOffset (Byte, Si, 0)), Reg Al)) in
			inst3 :: inst2 :: inst1 :: acc0
		| Word ->
			let acc0 = load Ax op func_depth acc in
			(Inst (Push (Reg Ax))) :: acc0
		| Tbyte ->
			let acc0 = loadReal op func_depth acc in
			let inst1 = Inst (Sub (Reg Sp, Imm (Int sizeOfType typ))) in
			let inst2 = Inst (Mov (Reg Si, Reg Sp)) in
			let inst3 = Inst (Fstp (Mem (RegPlusOffset (Tbyte, Si, 0)))) in
			inst3 :: inst2 :: inst1 :: acc0
	in
	let outer_func_inf = 
		match outer_func.entry_info with
		| ENTRY_function inf ->
			inf
		| _ ->
			internal "It should be a function entry";
			raise Exit
	in
	let functions_vars_size = outer_func_inf.function_vars_size in
	let functions_body_label = outer_func_inf.function_body_label in
	let label = label_of_int qdLabel in
	let acc0 = (Label label) :: (Comment (qdLabel, quad)) :: acc in
	match quad with
	| Q_calculate (op1, oper, op2, l) -> begin
		let realCalculationAsm inst =
			let acc1 = loadReal op1 func_depth acc0 in
			let acc2 = loadReal op2 func_depth acc1 in
			storeReal l func_depth (inst :: acc2)
		in
		match oper with
		| ADD ->
			let acc1 = load Ax op1 func_depth acc0 in
			let acc2 = load Dx op2 func_depth acc1 in
			let acc3 = (Inst (Add (Reg Ax, Reg Dx))) :: acc2 in
			store Ax l func_depth acc3
		| FADD ->
			realCalculationAsm (Inst Faddp)
		| SUB ->
			let acc1 = load Ax op1 func_depth acc0 in
			let acc2 = load Dx op2 func_depth acc1 in
			let acc3 = (Inst (Sub (Reg Ax, Reg Dx))) :: acc2 in
			store Ax l func_depth acc3
		| FSUB ->
			realCalculationAsm (Inst Fsubp)
		| MUL ->
			let acc1 = load Ax op1 func_depth acc0 in
			let acc2 = load Cx op2 func_depth acc1 in
			let acc3 = (Inst (Imul (Reg Cx))) :: acc2 in
			store Ax l func_depth acc3
		| FMUL ->
			realCalculationAsm (Inst Fmulp)
		| DIV ->
			let acc1 = load Ax op1 func_depth acc0 in
			let acc2 = load Cx op2 func_depth ((Inst Cwd) :: acc1) in
			let acc3 = (Inst (Idiv (Reg Cx))) :: acc2 in
			store Ax l func_depth acc3
		| FDIV ->
			realCalculationAsm (Inst Fdivp)
		| MOD ->
			let acc1 = load Ax op1 func_depth acc0 in
			let acc2 = load Cx op2 func_depth ((Inst Cwd) :: acc1) in
			let acc3 = (Inst (Idiv (Reg Cx))) :: acc2 in
			store Dx l func_depth acc3
	  end
	| Q_compare (op1, rel, op2, t) -> begin
		let intCompareAsm inst =
			let acc1 = load Ax op1 func_depth acc0 in
			let acc2 = load Dx op2 func_depth acc1 in
			inst :: (Inst (Cmp (Reg Ax, Reg Dx))) :: acc2
		in
		let charCompareAsm inst =
			let acc1 = load Al op1 func_depth acc0 in
			let acc2 = load Dl op2 func_depth acc1 in
			inst :: (Inst (Cmp (Reg Al, Reg Dl))) :: acc2
		in
		let realCompareAsm asm_op inst =
			let acc1 = loadReal op1 func_depth acc0 in
			let acc2 = loadReal op2 func_depth acc1 in
			inst :: (Inst (Test (Reg Ax, asm_op))) :: (Inst Fcompp) :: acc2
		in
		match rel with
		| EQ ->
			intCompareAsm (Inst (Je (label_of_int t)))
		| CEQ ->
			charCompareAsm (Inst (Je (label_of_int t)))
		| FEQ ->
			realCompareAsm (Imm (Hex "4000")) (Inst (Jnz (label_of_int t)))
		| NEQ ->
			intCompareAsm (Inst (Jne (label_of_int t)))
		| CNEQ ->
			charCompareAsm (Inst (Jne (label_of_int t)))
		| FNEQ ->
			realCompareAsm (Imm (Hex "4000")) (Inst (Jz (label_of_int t)))
		| GE ->
			intCompareAsm (Inst (Jg (label_of_int t)))
		| CGE ->
			charCompareAsm (Inst (Jg (label_of_int t)))
		| FGE ->
			realCompareAsm (Imm (Hex "0100")) (Inst (Jnz (label_of_int t)))
		| LE ->
			intCompareAsm (Inst (Jl (label_of_int t)))
		| CLE ->
			charCompareAsm (Inst (Jl (label_of_int t)))
		| FLE ->
			realCompareAsm (Imm (Hex "4500")) (Inst (Jz (label_of_int t)))
		| GEQ ->
			intCompareAsm (Inst (Jge (label_of_int t)))
		| CGEQ ->
			charCompareAsm (Inst (Jge (label_of_int t)))
		| FGEQ ->
			realCompareAsm (Imm (Hex "4500")) (Inst (Jnz (label_of_int t)))
		| LEQ ->
			intCompareAsm (Inst (Jle (label_of_int t)))
		| CLEQ ->
			charCompareAsm (Inst (Jle (label_of_int t)))
		| FLEQ ->
			realCompareAsm (Imm (Hex "0100")) (Inst (Jz (label_of_int t)))
	  end
	| Q_ifb (op, t) ->
		let acc1 = load Al op func_depth acc0 in
		(Inst (Jnz (label_of_int t))) :: (Inst (Or (Reg Al, Reg Al))) :: acc1
	| Q_jump t ->
		(Inst (Jmp (label_of_int t))) :: acc0
	| Q_unit e ->
		(* label sto opoio tha kanoun jump tuxon tail rec calls *)
		outer_func_inf.function_body_label <- label_of_int (qdLabel + 1);
		let inst1 = Inst (Proc (name e)) in
		let inst2 = Inst (Push (Mem (VarPlusOffset (Word, "display", 2 * func_depth)))) in
		let inst3 = Inst (Push (Reg Bp)) in
		let inst4 = Inst (Mov (Reg Bp, Reg Sp)) in
		let inst5 = Inst (Mov (Mem (VarPlusOffset (Word, "display", 2 * func_depth)), Reg Bp)) in
		let inst6 = Inst (Sub (Reg Sp, Imm (Int functions_vars_size))) in
		inst6 :: inst5 :: inst4 :: inst3 :: inst2 :: inst1 :: acc0
	| Q_endu e ->
		let inst1 = Inst (Mov (Reg Sp, Reg Bp)) in
		let inst2 = Inst (Pop (Reg Bp)) in
		let inst3 = Inst (Pop (Mem (VarPlusOffset (Word, "display", 2 * func_depth)))) in
		let inst4 = Inst Ret in
		let inst5 = Inst (Endp (name e)) in
		inst5 :: inst4 :: inst3 :: inst2 :: inst1 :: acc0
	| Q_array (e, op, l) ->
		let (size, dims) =
			match (getEntryTyp e) with
			| TYPE_array (tau, d) ->
				(sizeOfType tau, d)
			| _ ->
				internal "It should be of type array\n";
				raise Exit
		in
		let acc1 = load Ax op func_depth acc0 in
		let inst1 = Inst (Mov (Reg Cx, Imm (Int size))) in
		let inst2 = Inst (Imul (Reg Cx)) in
		(* epeidi i llama vazei ta dims prin ton pinaka *)
		let inst3 = Inst (Add (Reg Ax, Imm (Int ((sizeOfType TYPE_int) * dims)))) in
		let acc2 = load Cx (OP_left (Left_entry e)) func_depth (inst3 :: inst2 :: inst1 :: acc1) in
		let inst4 = Inst (Add (Reg Ax, Reg Cx)) in
		store Ax l func_depth (inst4 :: acc2)
	| Q_dim (e, d, l) ->
		let acc1 = load Si (OP_left (Left_entry e)) func_depth acc0 in
		let isnt1 =
			Inst (Mov (Reg Ax, Mem (RegPlusOffset (Word, Si, (sizeOfType TYPE_int) * (d - 1)))))
		in
		store Ax l func_depth (isnt1 :: acc1)
	| Q_assign (op, l) -> begin
		let typ = getLeftTyp l in
		let ptr_type = ptr_type_of_type typ in
		match ptr_type with
		| Byte ->
			let acc1 = load Al op func_depth acc0 in
			store Al l func_depth acc1
		| Word ->
			let acc1 = load Ax op func_depth acc0 in
			store Ax l func_depth acc1
		| Tbyte ->
			let acc1 = loadReal op func_depth acc0 in
			storeReal l func_depth acc1
	  end
	| Q_return op -> begin
		let inst1 = Inst (Mov (Reg Si, Mem (RegPlusOffset (Word, Bp, 6)))) in
		let typ = getOPTyp op in
		let ptr_type = ptr_type_of_type typ in
		match ptr_type with
		| Byte ->
			let acc1 = load Al op func_depth acc0 in
			let inst2 = Inst (Mov (Mem (RegPlusOffset (Byte, Si, 0)), Reg Al)) in
			inst2 :: inst1 :: acc1
		| Word ->
			let acc1 = load Ax op func_depth acc0 in
			let inst2 = Inst (Mov (Mem (RegPlusOffset (Word, Si, 0)), Reg Ax)) in
			inst2 :: inst1 :: acc1
		| Tbyte ->
			let acc1 = loadReal op func_depth acc0 in
			let inst2 = Inst (Fstp (Mem (RegPlusOffset (Tbyte, Si, 0)))) in
			inst2 :: inst1 :: acc1
	  end
	| Q_call (l, parl, r) -> begin
		let acc1 = List.fold_left asm_of_par acc0 parl in
		let acc2 = 
			match r with
			| None ->
				acc1
			| Some l ->
				(Inst (Push (Reg Si))) :: (loadAddr Si l func_depth acc1)
		in
		let rec isProcedure tau =
			match tau with
			| TYPE_func (_, TYPE_unit) ->
				true
			| TYPE_func (_, resTyp) ->
				isProcedure resTyp
			| _ ->
				false
		in
		let rec parsSize tau =
			match tau with
			| TYPE_func (argTyp, resTyp) ->
				(sizeOfType argTyp) + (parsSize resTyp)
			| _ ->
				0
		in
		let typ = getLeftTyp l in
		let acc3 = 
			if (isProcedure typ) then (Inst (Sub (Reg Sp, Imm (Int 2)))) :: acc2
			else acc2
		in
		let acc4 =
			match l with
			| Left_entry e -> begin
				match e.entry_info with
				| ENTRY_function _ ->
					(Inst (Call (ProcName (name e)))) :: acc3
				| _ ->
					(Inst (Call (Reg Di))) :: (load Di (OP_left l) func_depth acc3)
			  end
			| Left_deref _ ->
				(Inst (Call (Reg Di))) :: (load Di (OP_left l) func_depth acc3)
		in
		(Inst (Add (Reg Sp, Imm (Int ((parsSize typ) + 2))))) :: acc4
	  end
	| Q_tailRecCall (e, parl) ->
		let acc1 = List.fold_left asm_of_par acc0 parl in
		let copyPar (acc, offset) (Par p) =
			let typ = getOPTyp p in
			let size = sizeOfType typ in
			let ptr_type = ptr_type_of_type typ in
			match ptr_type with
			| Byte ->
				let inst1 = Inst (Mov (Reg Si, Reg Sp)) in
				let inst2 = Inst (Mov (Reg Al, Mem (RegPlusOffset (Byte, Si, 0)))) in
				let inst3 = Inst (Add (Reg Sp, Imm (Int size))) in
				let inst4 = Inst (Mov (Mem (RegPlusOffset (Byte, Bp, offset)), Reg Al)) in
				(inst4 :: inst3 :: inst2 :: inst1 :: acc, size + offset)
			| Word ->
				let inst1 = Inst (Pop (Mem (RegPlusOffset (Word, Bp, offset)))) in
				(inst1 :: acc, size + offset)
			| Tbyte ->
				let inst1 = Inst (Mov (Reg Si, Reg Sp)) in
				let inst2 = Inst (Fld (Mem (RegPlusOffset (Tbyte, Si, 0)))) in
				let inst3 = Inst (Add (Reg Sp, Imm (Int size))) in
				let inst4 = Inst (Fstp (Mem (RegPlusOffset (Tbyte, Bp, offset)))) in
				(inst4 :: inst3 :: inst2 :: inst1 :: acc, size + offset)
		in
		let acc2 = fst (List.fold_left copyPar (acc1, start_positive_offset) (List.rev parl)) in
		(Inst (Jmp functions_body_label)) :: acc2
		

let generateAsm quadListList =
	let asm_of_quadList (m, acc) quadList =
		match (List.hd quadList) with
		| (_, Q_unit outer_func) ->
			let newAcc = List.fold_left (asm_of_quad outer_func) acc quadList in
			let newM = max m (outer_func.entry_nesting + 1) in
			(newM, newAcc)
		| _ ->
			internal "It should be a unit quad";
			raise Exit
	in
	let (maxDepth, acc0) = List.fold_left asm_of_quadList (1, []) quadListList in
	let acc1 = StringMap.fold (fun _ extrn acc -> extrn @ acc) !libFuncsWH acc0 in
	let acc2 = (Inst (Array ("display", maxDepth))) :: acc1 in
	let acc3 = StringMap.fold (fun _ (_, str) acc -> str @ acc) !strWH acc2 in
	let acc4 = FloatMap.fold (fun _ (_, real) acc -> real @ acc) !realWH acc3 in
	List.rev acc4



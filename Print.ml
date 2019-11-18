open Format
open Identifier
open Types
open Symbol
open Structs
open Error
open Quads
open Asm


let show_offsets = true


let printSymbolTable ff =
	let rec walk ppf scp =
		if scp.sco_nesting <> 0 then begin
			fprintf ppf "scope: ";
			let rec entry ppf e =
				fprintf ppf "%a" pretty_id e.entry_id;
				match e.entry_info with
				| ENTRY_variable inf ->
					if show_offsets then
						fprintf ppf "[%d] : %a" 
							inf.variable_offset
							pretty_typ inf.variable_type
				| ENTRY_function inf ->
					let param ppf e =
						match e.entry_info with
						| ENTRY_parameter inf ->
							fprintf ppf "%a : %a"
								entry e
								pretty_typ inf.parameter_type
						| _ ->
							fprintf ppf "<invalid>"
					in
					let var ppf e =
						match e.entry_info with
						| ENTRY_variable inf ->
							fprintf ppf "%a"
								entry e
						| _ ->
							fprintf ppf "<invalid>"
					in
					let temp ppf e =
						match e.entry_info with
						| ENTRY_temporary inf ->
							fprintf ppf "%a : %a"
							entry e
							pretty_typ inf.temporary_type
						| _ ->
							fprintf ppf "<invalid>"
					in
					let rec entries ppf (ef, ps) =
						match ps with
						| [p] ->
							fprintf ppf "%a" ef p
						| p :: ps ->
							fprintf ppf "%a; %a" ef p entries (ef, ps);
						| [] ->
							() 
					in
					fprintf ppf "(%a) : %a\n\tmy vars: (%a)\n\tmy temps: (%a)\n\tsize: %d,\tdepth: %d"
						entries (param, inf.function_paramlist)
						pretty_typ inf.function_type
						entries (var, inf.function_varlist)
						entries (temp, inf.function_templist)
						inf.function_vars_size
						e.entry_nesting
				| ENTRY_parameter inf ->
					if show_offsets then
						fprintf ppf "[%d]" inf.parameter_offset
				| ENTRY_temporary inf ->
					if show_offsets then
						fprintf ppf "[%d]" inf.temporary_offset
				| ENTRY_constructor _
				| ENTRY_type _ ->
					() 
			in
			let rec entries ppf es =
				match es with
				| [e] ->
					fprintf ppf "%a" entry e
				| e :: es ->
					fprintf ppf "%a, %a" entry e entries es
				| [] ->
					()
			in
			match scp.sco_parent with
			| Some scpar ->
				fprintf ppf "%a\n%a"
					entries scp.sco_entries
					walk scpar
			| None ->
				fprintf ppf "<impossible>\n"
		end
	in
	let scope ppf scp =
		if scp.sco_nesting == 0 then
			fprintf ppf "no scope\n"
		else
			walk ppf scp 
	in
	fprintf ff "%a----------------------------------------\n"
		scope !currentScope


let labelMap = ref IntMap.empty


let printOperator ppf oper =
	match oper with
	| FADD
	| ADD ->
		fprintf ppf "+"
	| FSUB
	| SUB ->
		fprintf ppf "-"
	| FMUL
	| MUL ->
		fprintf ppf "*"
	| FDIV
	| DIV ->
		fprintf ppf "/"
	| MOD ->
		fprintf ppf "%%"


let printRelation ppf rel =
	match rel with
	| FEQ
	| CEQ
	| EQ ->
		fprintf ppf "="
	| FNEQ
	| CNEQ
	| NEQ ->
		fprintf ppf "<>"
	| FGE
	| CGE
	| GE ->
		fprintf ppf ">"
	| FLE
	| CLE
	| LE ->
		fprintf ppf "<"
	| FGEQ
	| CGEQ
	| GEQ ->
		fprintf ppf ">="
	| FLEQ
	| CLEQ
	| LEQ ->
		fprintf ppf "<="


let printLeft ppf l =
	match l with
	| Left_entry e ->
		fprintf ppf "%s" e.entry_name
	| Left_deref e ->
		fprintf ppf "[%s]" e.entry_name


let printConstant ppf c =
	match c with
	| Const_int i ->
		fprintf ppf "%d" i
	| Const_float f ->
		fprintf ppf "%f" f
	| Const_bool b ->
		fprintf ppf "%B" b
	| Const_char c ->
		fprintf ppf "%C" c
	| Const_string s ->
		fprintf ppf "%S" s


let printOperand ppf op =
	match op with
	| OP_constant c ->
		fprintf ppf "%a" printConstant c
	| OP_left l ->
		fprintf ppf "%a" printLeft l


let printQuad ff (label, quad) =
	let printRes ppf (tag, r) =
		fprintf ppf "%d:\tpar, %a, RET, -\n" tag printLeft r
	in
	let printPars ppf parl =
		let printPar tag (Par p) =
			fprintf ppf "%d:\tpar, %a, V, -\n" tag printOperand p;
			(tag + 1)
		in
		ignore (List.fold_left printPar label parl)
	in
	match quad with
	| Q_unit e ->
		fprintf ff "%d:\tunit, %s, -, -" label e.entry_name
	| Q_endu e ->
		fprintf ff "%d:\tendu, %s, -, -" label e.entry_name
	| Q_jump tag ->
		let tag = try IntMap.find tag !labelMap with Not_found -> tag in
		fprintf ff "%d:\tjump, -, -, %d" label tag
	| Q_ifb (op, tag) ->
		let tag = try IntMap.find tag !labelMap with Not_found -> tag in
		fprintf ff "%d:\tifb, %a, -, %d" label printOperand op tag
	| Q_compare (op1, rel, op2, tag) ->
		let tag = try IntMap.find tag !labelMap with Not_found -> tag in
		fprintf ff "%d:\t%a, %a, %a, %d" 
			label printRelation rel printOperand op1 printOperand op2 tag
	| Q_calculate (op1, oper, op2, l) ->
		fprintf ff "%d:\t%a, %a, %a, %a" 
			label printOperator oper printOperand op1 printOperand op2 printLeft l
	| Q_assign (op, l) ->
		fprintf ff "%d:\t:=, %a, -, %a" label printOperand op printLeft l
	| Q_array (e, op, l) ->
		fprintf ff "%d:\tarray, %s, %a, %a" label e.entry_name printOperand op printLeft l
	| Q_dim (e, d, l) ->
		fprintf ff "%d:\tdim, %s, %d, %a" label e.entry_name d printLeft l
	| Q_call (l, parl, None) ->
		fprintf ff "%a%d:\tcall, -, -, %a" 
			printPars parl (label + List.length parl) printLeft l
	| Q_call (l, parl, Some r) -> 
		fprintf ff "%a%a%d:\tcall, -, -, %a"
			printPars parl printRes (label + (List.length parl), r)
			(label + List.length parl + 1) printLeft l
	| Q_tailRecCall (e, parl) ->
		fprintf ff "%a%d:\ttail rec call, -, -, %s" printPars parl (label + List.length parl) e.entry_name
	| Q_return op ->
		fprintf ff "%d:\t:=, %a, -, $$" label printOperand op


let printQuads ff qdsListList =
	let functionsQuads ppf qL =
		List.iter (fun q -> fprintf ppf "%a\n" printQuad q) qL
	in
	let rec programQuads ppf qLL =
		match qLL with
		| [qL] ->
			fprintf ppf "%a" functionsQuads qL
		| (qL :: qLs) ->
			fprintf ppf "%a\n%a" functionsQuads qL programQuads qLs
		| [] ->
			()
	in
	let constructMap qLL =
		let processFunction (initLab, initMap, acc) funcQuads =
			let processQuad (newLabel, map, funcAcc) (exLabel, quad) =
				let newMap = IntMap.add exLabel newLabel map in
				let newQ = (newLabel, quad) in
				match quad with
				| Q_call (_, parl, None) ->
					(newLabel + (List.length parl) + 1, newMap, newQ :: funcAcc)
				| Q_tailRecCall (_, parl) ->
					(newLabel + (List.length parl) + 1, newMap, newQ :: funcAcc)
				| Q_call (_, parl, Some _) ->
					(newLabel + (List.length parl) + 2, newMap, newQ :: funcAcc)
				| _ ->
					(newLabel + 1, newMap, newQ :: funcAcc)
			in
			let (nextLab, map, funcQds) = List.fold_left processQuad (initLab, initMap, []) funcQuads in
			(nextLab, map, List.rev funcQds :: acc)
		in
		let (_, map, programQds) = List.fold_left processFunction (1, IntMap.empty, []) qLL in
		labelMap := map;
		List.rev programQds
	in
	let programQds = constructMap qdsListList in
	fprintf ff "%a" programQuads programQds;
	labelMap := IntMap.empty


let printMyQuad ff (label, quad) =
	let rec printPars ppf parl =
		match parl with
		| [Par p] ->
			fprintf ppf "%a}" printOperand p
		| (Par p) :: pars ->
			fprintf ppf "%a, %a" printOperand p printPars pars
		| [] ->
			fprintf ppf "}"
	in
	match quad with
	| Q_call (l, parl, None) ->
		fprintf ff "%d:\tcall, {%a, -, %a" 
			label printPars parl printLeft l
	| Q_call (l, parl, Some r) ->
		fprintf ff "%d:\tcall, {%a, %a, %a"
			label printPars parl printLeft r printLeft l
	| Q_tailRecCall (e, parl) ->
		fprintf ff "%d:\ttail rec call, {%a, -, %s"
			label printPars parl e.entry_name
	| _ ->
		printQuad ff (label, quad)


let printAsm ff asmList =
	let printReg ppf reg =
		match reg with
		| Ax ->
			fprintf ppf "ax"
		| Al ->
			fprintf ppf "al"
		| Bx ->
			fprintf ppf "bx"
		| Cx ->
			fprintf ppf "cx"
		| Dx ->
			fprintf ppf "dx"
		| Dl ->
			fprintf ppf "dl"
		| Di ->
			fprintf ppf "di"
		| Si ->
			fprintf ppf "si"
		| Bp ->
			fprintf ppf "bp"
		| Sp ->
			fprintf ppf "sp"
	in
	let printPtrType ppf ptr_type =
		match ptr_type with
		| Byte ->
			fprintf ppf "byte"
		| Word ->
			fprintf ppf "word"
		| Tbyte ->
			fprintf ppf "tbyte"
	in
	let printOp ppf op =
		match op with
		| Reg reg ->
			fprintf ppf "%a" printReg reg
		| Mem (RegPlusOffset (ptr_type, reg, off)) ->
			fprintf ppf "%a ptr [%a + (%d)]" printPtrType ptr_type printReg reg off
		| Mem (VarPlusOffset (ptr_type, name, off)) ->
			fprintf ppf "%a ptr [%s + (%d)]" printPtrType ptr_type name off
		| Imm (Int v) ->
			fprintf ppf "%d" v
		| Imm (Hex h) ->
			fprintf ppf "%sh" h
		| ProcName name ->
			fprintf ppf "near ptr %s" name
	in
	let printLine ppf line = 
		match line with
		| Inst instr -> begin
			match instr with
			| Mov (op1, op2) ->
				fprintf ppf "\tmov\t%a, %a" printOp op1 printOp op2
			| Add (op1, op2) ->
				fprintf ppf "\tadd\t%a, %a" printOp op1 printOp op2
			| Sub (op1, op2) ->
				fprintf ppf "\tsub\t%a, %a" printOp op1 printOp op2
			| Idiv op ->
				fprintf ppf "\tidiv\t%a" printOp op
			| Imul op ->
				fprintf ppf "\timul\t%a" printOp op
			| Cwd ->
				fprintf ppf "\tcwd"
			| Faddp ->
				fprintf ppf "\tfaddp\tST(1), ST(0)"
			| Fsubp ->
				fprintf ppf "\tfsubp\tST(1), ST(0)"
			| Fmulp ->
				fprintf ppf "\tfmulp\tST(1), ST(0)"
			| Fdivp ->
				fprintf ppf "\tfdivp\tST(1), ST(0)"
			| Cmp (op1, op2) ->
				fprintf ppf "\tcmp\t%a, %a" printOp op1 printOp op2
			| Fcompp ->
				fprintf ppf "\tfcompp"
			| Test (op1, op2) ->
				fprintf ppf "\ttest%a, %a" printOp op1 printOp op2
			| Jmp t ->
				fprintf ppf "\tjmp\t%s" t
			| Je t ->
				fprintf ppf "\tje\t%s" t
			| Jne t ->
				fprintf ppf "\tjne\t%s" t
			| Jg t ->
				fprintf ppf "\tjg\t%s" t
			| Jl t ->
				fprintf ppf "\tjl\t%s" t
			| Jge t ->
				fprintf ppf "\tjge\t%s" t
			| Jle t ->
				fprintf ppf "\tjle\t%s" t
			| Jz t ->
				fprintf ppf "\tjz\t%s" t
			| Jnz t ->
				fprintf ppf "\tjnz\t%s" t
			| Or (op1, op2) ->
				fprintf ppf "\tor\t%a, %a" printOp op1 printOp op2
			| Proc name ->
				fprintf ppf "%s\tproc\tnear" name
			| Endp name ->
				fprintf ppf "%s\tendp" name
			| Ret ->
				fprintf ppf "\tret"
			| Push op ->
				fprintf ppf "\tpush\t%a" printOp op
			| Pop op ->
				fprintf ppf "\tpop\t%a" printOp op
			| Lea (op1, op2) ->
				fprintf ppf "\tlea\t%a, %a" printOp op1 printOp op2
			| Fld op ->
				fprintf ppf "\tfld\t%a" printOp op
			| Fstp op ->
				fprintf ppf "\tfstp\t%a" printOp op
			| Call op ->
				fprintf ppf "\tcall\t%a" printOp op
			| Extrn func ->
				fprintf ppf "\textrn\t%s : proc" func
			| Array (name, size) ->
				fprintf ppf "%s\tdw\t%d dup(0)" name size
			| Db v ->
				fprintf ppf "\tdb\t%d" v
			| String (name, size) ->
				fprintf ppf "%s\tdw\t%d" name size
			| Real (name, f) ->
				fprintf ppf "%s\tdt\t%f" name f
		  end			
		| Label t ->
			fprintf ppf "%s:" t
		| Comment (l, qd) ->
			fprintf ppf "\n;%a\n" printMyQuad (l, qd)
	in
	fprintf ff "xseg\tsegment\tpublic\t'code'\n \
		\tassume\tcs:xseg, ds:xseg, ss:xseg\n \
		\torg\t100h\n \
		main\tproc\tnear\n \
		\tcall\tnear ptr __outer\n \
		\tmov\tax, 4C00h\n \
		\tint\t21h\n \
		main\tendp\n";
	List.iter (fun line -> fprintf ff "%a\n" printLine line) asmList;
	fprintf ff "xseg\tends\n \
		\tend\tmain\n"




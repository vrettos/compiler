open Quads
open Types
open Error

type asm_operand = 
	| Reg of register
	| Mem of memory
	| Imm of immediate
	| ProcName of string
and register = 
	| Ax
	| Al
	| Bx
	| Cx
	| Dx
	| Dl
	| Di
	| Si
	| Bp
	| Sp
and immediate =
	| Int of int
	| Hex of string
and memory =
	| RegPlusOffset of ptr_type * register * int
	| VarPlusOffset of ptr_type * string * int
and ptr_type =
	| Byte
	| Word
	| Tbyte
and instruction =
	| Mov of asm_operand * asm_operand
	| Add of asm_operand * asm_operand
	| Sub of asm_operand * asm_operand
	| Idiv of asm_operand
	| Imul of asm_operand
	| Cwd
	| Faddp
	| Fsubp
	| Fmulp
	| Fdivp
	| Cmp of asm_operand * asm_operand
	| Fcompp
	| Test of asm_operand * asm_operand
	| Jmp of string
	| Je of string
	| Jne of string
	| Jg of string
	| Jl of string
	| Jge of string
	| Jle of string
	| Jz of string
	| Jnz of string
	| Or of asm_operand * asm_operand
	| Proc of string
	| Endp of string
	| Ret
	| Push of asm_operand
	| Pop of asm_operand
	| Db of int
	| Lea of asm_operand * asm_operand
	| Fld of asm_operand
	| Fstp of asm_operand
	| Call of asm_operand
	| Extrn of string
	| Array of string * int
	| String of string * int
	| Real of string * float
and line =
	| Inst of instruction
	| Label of string
	| Comment of int * quad
and program = line list


let ptr_type_of_reg reg =
	match reg with
	| Al
	| Dl ->
		Byte
	| _ ->
		Word


let ptr_type_of_type t =
	match t with
	| TYPE_bool ->
		Byte
	| TYPE_char ->
		Byte
	| TYPE_int ->
		Word
	| TYPE_float ->
		Tbyte
	| TYPE_array _ ->
		Word
	| TYPE_ref _ ->
		Word
	| TYPE_func _ ->
		Word
	| TYPE_unit
	| TYPE_id _
	| TYPE_var _ ->
		internal "Should not be a pointer to unit";
		raise Exit


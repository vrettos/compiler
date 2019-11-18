open Error
open Print
open Types

let errorMsg p t1 t2 =
	fatal "Type mismatch:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		but type:\t%a\t was expected" 
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t1 pretty_typ t2


let noPolyMsg p t =
	fatal "Polymorphic type:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		but polymorphism is not supported"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t


let refToArrayMsg p t =
	fatal "Reference to array:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
                but reference to array is prohibited"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t


let arrayOfArraysMsg p t =
	fatal "Array of arrays:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		but array of arrays is prohibited"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t


let funcRetArrayMsg p t =
	fatal "Function returning array:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		but function returning array is prohibited"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t


let retFuncMsg p t resT =
	fatal "Function returning function:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		and it is returning an expression\n \
		of type:\t%a\n \
		but function returning function is prohibited"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t pretty_typ resT


let noUdtTypes () =
	fatal "User defined types are not currently supported"


let dimMsg p t dim = 
	fatal "Wrong dimension message:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		but you are requesting the size of the %d dimension"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t dim


let eqMsg p t =
	fatal "Invalid argument:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		but type %a and type %a are prohibited"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t pretty_typ (TYPE_array (TYPE_var 1, 1))
			pretty_typ (TYPE_func (TYPE_var 1, TYPE_var 2))


let lessMsg p t =
	fatal "Invalid argument:\n \
		Expression starting at line: %d, character: %d and ending at line: %d, character: %d\n \
		has type:\t%a\n \
		but type:\t%a or %a or %a was expected"
			(fst p).Lexing.pos_lnum ((fst p).Lexing.pos_cnum - (fst p).Lexing.pos_bol)
			(snd p).Lexing.pos_lnum ((snd p).Lexing.pos_cnum - (snd p).Lexing.pos_bol)
			pretty_typ t pretty_typ TYPE_int pretty_typ TYPE_float pretty_typ TYPE_char


let loopForeverMsg p name =
	warning "Loop forever:\n \
		Function %s declared at line: %d\n \
		is going to loop forever"
			name (fst p).Lexing.pos_lnum



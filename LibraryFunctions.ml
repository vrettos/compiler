open Symbol
open Types

let addLibrary () =
	ignore (newLibraryFunction "print_int" (TYPE_func (TYPE_int, TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "print_bool" (TYPE_func (TYPE_bool, TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "print_char" (TYPE_func (TYPE_char, TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "print_float" (TYPE_func (TYPE_float, TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "print_string" (TYPE_func ((TYPE_array(TYPE_char, 1)), TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "read_int" (TYPE_func (TYPE_unit, TYPE_int)) TYPE_int true);
	ignore (newLibraryFunction "read_bool" (TYPE_func (TYPE_unit, TYPE_bool)) TYPE_bool true);
	ignore (newLibraryFunction "read_char" (TYPE_func (TYPE_unit, TYPE_char)) TYPE_char true);
	ignore (newLibraryFunction "read_float" (TYPE_func (TYPE_unit, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "read_string" (TYPE_func ((TYPE_array(TYPE_char, 1)), TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "abs" (TYPE_func (TYPE_int, TYPE_int)) TYPE_int true);
	ignore (newLibraryFunction "fabs" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "sqrt" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "sin" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "cos" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "tan" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "atan" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true);
	let entry_exp = newLibraryFunction "exp" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true in
	let entry_ln = newLibraryFunction "ln" (TYPE_func (TYPE_float, TYPE_float)) TYPE_float true in
	ignore (newLibraryFunction "pi" (TYPE_func (TYPE_unit, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "incr" (TYPE_func ((TYPE_ref TYPE_int), TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "decr" (TYPE_func ((TYPE_ref TYPE_int), TYPE_unit)) TYPE_unit true);
	ignore (newLibraryFunction "float_of_int" (TYPE_func (TYPE_int, TYPE_float)) TYPE_float true);
	ignore (newLibraryFunction "int_of_float" (TYPE_func (TYPE_float, TYPE_int)) TYPE_int true);
	ignore (newLibraryFunction "round" (TYPE_func (TYPE_float, TYPE_int)) TYPE_int true);
	ignore (newLibraryFunction "int_of_char" (TYPE_func (TYPE_char, TYPE_int)) TYPE_int true);
	ignore (newLibraryFunction "char_of_int" (TYPE_func (TYPE_int, TYPE_char)) TYPE_char true);
	ignore (newLibraryFunction "strlen" (TYPE_func (TYPE_array (TYPE_char, 1), TYPE_int)) TYPE_int true);
	ignore (newLibraryFunction "strcmp" (TYPE_func (TYPE_array (TYPE_char, 1),
		TYPE_func (TYPE_array (TYPE_char, 1), TYPE_int))) TYPE_int true);
	ignore (newLibraryFunction "strcpy" (TYPE_func (TYPE_array (TYPE_char, 1),
		TYPE_func (TYPE_array (TYPE_char, 1), TYPE_unit))) TYPE_unit true);
	ignore (newLibraryFunction "strcat" (TYPE_func (TYPE_array (TYPE_char, 1),
		TYPE_func (TYPE_array (TYPE_char, 1), TYPE_unit))) TYPE_unit true);
	(entry_ln, entry_exp)


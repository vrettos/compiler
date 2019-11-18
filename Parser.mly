%{
	open Ast
	open Types
%}

%token  T_eof
%token  <int>  T_iconst
%token  <float>  T_fconst
%token  <char>  T_cconst
%token  <string>  T_sconst
%token  <string>  T_name
%token  <string>  T_constrname
%token  T_dim
%token  T_false
%token  T_let
%token  T_of
%token  T_type
%token  T_array
%token  T_do
%token  T_float
%token  T_match
%token  T_rec
%token  T_unit
%token  T_bool
%token  T_downto
%token  T_if
%token  T_mutable
%token  T_then
%token  T_with
%token  T_begin
%token  T_done
%token  T_for
%token  T_mod
%token  T_ref
%token  T_while
%token  T_char
%token  T_else
%token  T_in
%token  T_new
%token  T_to
%token  T_delete
%token  T_end
%token  T_int
%token  T_not
%token  T_true
%token  T_structneq
%token  T_ls
%token  T_gr
%token  T_leq
%token  T_geq
%token  T_naturaleq
%token  T_naturalneq
%token  T_assign
%token  T_fminus
%token  T_ftimes
%token  T_fdiv
%token  T_fpower
%token  T_dembanger
%token  T_semicolumn
%token  T_mand
%token  T_and
%token  T_or
%token  T_arrow
%token  T_structeq
%token  T_vertbar
%token  T_plus
%token  T_minus
%token  T_times
%token  T_div
%token  T_fplus
%token  T_lparen
%token  T_rparen
%token  T_lbracket
%token  T_rbracket
%token  T_comma
%token  T_colon


%nonassoc T_in
%left T_semicolumn
%nonassoc T_if
%nonassoc T_then
%nonassoc T_else
%nonassoc T_assign
%left T_or
%left T_and
%nonassoc T_structeq T_structneq T_gr T_ls T_leq T_geq T_naturaleq T_naturalneq
%left T_plus T_minus T_fminus T_fplus
%left T_times T_div T_ftimes T_fdiv T_mod
%right T_fpower
%nonassoc T_not T_delete T_sign
%right T_dembanger
%right T_new
%right T_arrow
%nonassoc T_of
%nonassoc T_ref



%start program
%type <Ast.program> program

%%

program   	: letdeflist { AST_program $1 }
		;


letdeflist	: T_eof { [] }
		| letdef letdeflist { (AST_letdef $1) :: $2 }
		| typedef letdeflist { (AST_typedef $1) :: $2 }
		;


letdef 	  	: T_let deflist { AST_let (false, $2)}
          	| T_let T_rec deflist { AST_let (true, $3) }
		;


deflist		: def { [$1] }
		| def T_mand deflist { $1 :: $3 }
		;


typedef         : T_type tdeflist { AST_type $2 }
                ;


tdeflist	: tdef { [$1] }
                | tdef T_mand tdeflist{ $1 :: $3 }
                ;


tdef            : T_name T_structeq constrlist { AST_tdef ($1, $3) }
                ;

constrlist      : constr { [$1] }
                | constr T_vertbar constrlist { $1 :: $3 }
		;


def	  	: T_name paramlist typan T_structeq expr1 { {de = AST_def_normal ($1, $2, $3, $5); 
						dpos = (rhs_start_pos 1, rhs_end_pos 2)} 
									}
	  	| T_mutable T_name typan { {de = AST_def_mutable ($2, [], $3); dpos = (rhs_start_pos 2, rhs_end_pos 2)}}
	  	| T_mutable T_name T_lbracket exprlist T_rbracket typan { {de = AST_def_mutable ($2, $4, $6); 
						dpos = (rhs_start_pos 2, rhs_end_pos 5)} 
									}
		;


typan		: /* nothing */ { Types.newTypeVar () }
		| T_colon typos { $2 }
		;


exprlist 	: expr1 { [$1] }
	  	| expr1 T_comma exprlist { $1 :: $3 }
		;


paramlist	: /* nothing */ { [] }
	  	| par paramlist { $1 :: $2 }
		;


par	  	: T_name { AST_par ($1, Types.newTypeVar ()) }
	  	| T_lparen T_name  T_colon typos T_rparen { AST_par ($2, $4) }
		;


constr 	  	: T_constrname { AST_constr ($1, []) }
	  	| T_constrname T_of typoi { AST_constr ($1, $3)}
		;


typoi		: typos { [$1] }
	  	| typos typoi { $1 :: $2 }
		;


typos 	  	: T_unit { TYPE_unit }
	  	| T_int { TYPE_int }
	  	| T_char { TYPE_char }
	  	| T_bool { TYPE_bool }
	  	| T_float { TYPE_float }
	  	| T_lparen typos T_rparen { $2 }
	  	| typos T_arrow typos { TYPE_func ($1, $3) }
	  	| typos T_ref { TYPE_ref $1 }
 	  	| T_array T_lbracket dimensions T_rbracket T_of typos { TYPE_array ($6, $3) }
		| T_array T_of typos { TYPE_array ($3, 1) }
	  	| T_name { TYPE_id $1 }
		;


dimensions 	: T_times { 1 }
	  	| T_times T_comma dimensions { 1 + $3  }
		;


expr1	 	: T_plus expr1 %prec T_sign { {ex = AST_expr_unop (Positive, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| T_minus expr1 %prec T_sign { {ex = AST_expr_unop (Negative, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| T_fplus expr1 %prec T_sign { {ex = AST_expr_unop (Fpositive, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| T_fminus expr1 %prec T_sign { {ex = AST_expr_unop (Fnegative, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| T_not expr1 { {ex = AST_expr_unop (Not, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
	  	| expr1 T_plus expr1 { {ex = AST_expr_binop ($1, Add, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_minus expr1 { {ex = AST_expr_binop ($1, Sub, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_times expr1 { {ex = AST_expr_binop ($1, Mul, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_div expr1 { {ex = AST_expr_binop ($1, Div, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_fplus expr1 { {ex = AST_expr_binop ($1, Fadd, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_fminus expr1 { {ex = AST_expr_binop ($1, Fsub, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_ftimes expr1 { {ex = AST_expr_binop ($1, Fmul, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_fdiv expr1 { {ex = AST_expr_binop ($1, Fdiv, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_mod expr1 { {ex = AST_expr_binop ($1, Mod, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_fpower expr1 { {ex = AST_expr_binop ($1, Power, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_naturaleq expr1 { {ex = AST_expr_binop ($1, Naturaleq, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_naturalneq expr1 { {ex = AST_expr_binop ($1, Naturalneq, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_ls expr1 { {ex = AST_expr_binop ($1, Less, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_gr expr1 { {ex = AST_expr_binop ($1, Greater, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_leq expr1 { {ex = AST_expr_binop ($1, Leq, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_geq expr1 { {ex = AST_expr_binop ($1, Geq, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_structeq expr1 { {ex = AST_expr_binop ($1, Structeq, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_structneq expr1 { {ex = AST_expr_binop ($1, Structneq, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_and expr1 { {ex = AST_expr_binop ($1, And, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_or expr1 { {ex = AST_expr_binop ($1, Or, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_semicolumn expr1 { {ex = AST_expr_binop ($1, Semicolumn, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| expr1 T_assign expr1 { {ex = AST_expr_binop ($1, Assign, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
		| exprmiddle { $1 }
		| letdef T_in expr1 { {ex = AST_expr_letdef_in ($1, $3); epos = (rhs_start_pos 3, rhs_end_pos 3)} }
	  	| T_dim T_iconst T_name { {ex = AST_expr_dim ($2, $3); epos = (rhs_start_pos 1, rhs_end_pos 3)} }
	  	| T_dim T_name { {ex = AST_expr_dim (1, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
   	  	| T_delete expr1 { {ex = AST_expr_delete $2; epos = (rhs_start_pos 1, rhs_end_pos 2)} }
	  	| T_begin expr1 T_end { $2 }
	  	| T_if expr1 T_then expr1 { {ex = AST_expr_if ($2, $4, {ex = AST_expr_unit; epos = (rhs_end_pos 4, rhs_end_pos 4)}); 
					epos = (rhs_start_pos 1, rhs_end_pos 4)} }
	  	| T_if expr1 T_then expr1 T_else expr1 { {ex = AST_expr_if ($2, $4, $6); epos = (rhs_start_pos 1, rhs_end_pos 6)} }
	  	| T_while expr1 T_do expr1 T_done { {ex = AST_expr_while ($2, $4); epos = (rhs_start_pos 1, rhs_end_pos 5)} }
	  	| T_for T_name T_structeq expr1 T_to expr1 T_do expr1 T_done { 
					{ex = AST_expr_for ($2, $4, false, $6, $8); epos = (rhs_start_pos 1, rhs_end_pos 9)} 
									}
 	  	| T_for T_name T_structeq expr1 T_downto expr1 T_do expr1 T_done { 
					{ex = AST_expr_for ($2, $4, true, $6, $8); epos = (rhs_start_pos 1, rhs_end_pos 9)} 
				}
	  	| T_match expr1 T_with clauselist T_end { {ex = AST_expr_match ($2, $4); epos = (rhs_start_pos 1, rhs_end_pos 5)} }
		;


exprmiddle	: T_name expressions2 { {ex = AST_expr_function_call ($1, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| T_constrname expressions2 { {ex = AST_expr_constructor_call ($1, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| expr2 { $1 }
		;


expressions2    : expr2 { [$1] }
                | expr2 expressions2 { $1 :: $2 }
		;


expr2		: T_iconst { {ex = AST_expr_int_const $1; epos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_fconst { {ex = AST_expr_float_const $1; epos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_cconst { {ex = AST_expr_char_const $1; epos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_sconst { {ex = AST_expr_string_const $1; epos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_true { {ex = AST_expr_true; epos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_false { {ex = AST_expr_false; epos = (rhs_start_pos 1, rhs_end_pos 2)} }
                | T_lparen T_rparen { {ex = AST_expr_unit; epos = (rhs_start_pos 1, rhs_end_pos 2)} }
                | T_lparen expr1 T_rparen { $2 }
		| T_dembanger expr2 { {ex = AST_expr_unop (Dereference, $2); epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| T_new typos { {ex = AST_expr_new $2; epos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| T_name T_lbracket exprlist T_rbracket { {ex = AST_expr_matrix_element ($1, $3); epos = (rhs_start_pos 1, rhs_end_pos 4)} }
		| T_name { {ex = AST_expr_name $1; epos = (rhs_start_pos 1, rhs_end_pos 1)} }
		| T_constrname { {ex = AST_expr_Name $1; epos = (rhs_start_pos 1, rhs_end_pos 1)} }
		;


clause    	: pattern1 T_arrow expr1 { AST_clause ($1, $3) }
		;


clauselist	: clause { [$1] }
		| clause T_vertbar clauselist { $1 :: $3 }
		;


pattern1	: T_plus T_iconst { {pa = AST_pattern_int_const $2; ppos = (rhs_start_pos 1, rhs_end_pos 2)} }
	  	| T_minus T_iconst { {pa = AST_pattern_int_const (-$2); ppos = (rhs_start_pos 1, rhs_end_pos 2)} }
	  	| T_fplus T_fconst { {pa = AST_pattern_float_const $2; ppos = (rhs_start_pos 1, rhs_end_pos 2)} }
	  	| T_fminus T_fconst { {pa = AST_pattern_float_const (-.$2); ppos = (rhs_start_pos 1, rhs_end_pos 2)} }
	  	| patternmiddle { $1 }
		;


patternmiddle	: T_constrname patternlist { {pa = AST_pattern_constructor_call ($1, $2); ppos = (rhs_start_pos 1, rhs_end_pos 2)} }
		| pattern2 { $1 }
		;


pattern2	: T_iconst { {pa = AST_pattern_int_const $1; ppos = (rhs_start_pos 1, rhs_end_pos 1)} }
		| T_cconst { {pa = AST_pattern_char_const $1; ppos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_fconst { {pa = AST_pattern_float_const $1; ppos = (rhs_start_pos 1, rhs_end_pos 1)} }
		| T_true { {pa = AST_pattern_true; ppos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_false { {pa = AST_pattern_false; ppos = (rhs_start_pos 1, rhs_end_pos 1)} }
                | T_name { {pa = AST_pattern_name $1; ppos = (rhs_start_pos 1, rhs_end_pos 1)} }
		| T_lparen pattern1 T_rparen { $2 }
		| T_constrname { {pa = AST_pattern_Name $1; ppos = (rhs_start_pos 1, rhs_end_pos 1)} }
		;


patternlist  	: pattern2 { [$1] }
	  	| pattern2 patternlist { $1 :: $2 }
		;

 

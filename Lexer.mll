{
	open Parser

	let incr_linenum lexbuf =
		let pos = lexbuf.Lexing.lex_curr_p in
		lexbuf.Lexing.lex_curr_p <- { 
			pos with
				Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
				Lexing.pos_bol = pos.Lexing.pos_cnum;
		}

	(* Convert a string to a list of characters *)
	let explode s =
		let rec exp i l =
			if i < 0 then l 
			else exp (i - 1) (s.[i] :: l) 
		in
		exp (String.length s - 1) [];;

	(* Convert a list of characters to a string *)  
	let implode l =
		let res = String.create (List.length l) in
		let rec imp i = function
			| [] ->
				res
			| c :: l ->
				res.[i] <- c;
				imp (i + 1) l 
		in
		imp 0 l;;

	let getChar h1 h0 =
		let getValue x =
			if (('0' <= x) && (x <= '9')) then 
				(Char.code x) - (Char.code '0')
			else if (('A' <= x) && (x <= 'F')) then 
				(Char.code x) - (Char.code 'A') + 10
  			else 
				(Char.code x) -( Char.code 'a') + 10
		in
		Char.chr ((getValue h1) * 16 + (getValue h0))


}

let digit = ['0'-'9']
let hex = (digit | ['a'-'f' 'A'-'F'])
let letter = ['a'-'z' 'A'-'Z']
let white  = [' ' '\t' '\r']

rule lexer = parse
  | "and"	{ T_mand }
  | "dim"	{ T_dim }
  | "false"    	{ T_false }
  | "let"     	{ T_let }
  | "of"  	{ T_of }
  | "type"    	{ T_type }
  | "array"     { T_array }
  | "do"   	{ T_do }
  | "float"	{ T_float }
  | "match"	{ T_match }
  | "rec"	{ T_rec }
  | "unit"	{ T_unit }
  | "begin"	{ T_begin }
  | "done"	{ T_done }
  | "for"	{ T_for }
  | "mod"	{ T_mod }
  | "ref"	{ T_ref }
  | "while"	{ T_while }
  | "bool"	{ T_bool }
  | "downto"	{ T_downto }
  | "if"	{ T_if }
  | "mutable"	{ T_mutable }
  | "then"	{ T_then }
  | "with"	{ T_with }
  | "char"	{ T_char }
  | "else"	{ T_else }
  | "in"	{ T_in }
  | "new"	{ T_new }
  | "to"	{ T_to }
  | "delete"	{ T_delete }
  | "end"	{ T_end }
  | "int"	{ T_int }
  | "not"	{ T_not }
  | "true"	{ T_true }

  | digit+ as num	{ T_iconst (int_of_string num) }
  | digit+ '.' digit+ (('e' | 'E') ('+' | '-')? digit+)? as num		{ T_fconst (float_of_string num) } 
  | '\''	{ chars lexbuf }
  | '"'         { str [] lexbuf }
  | ['a'-'z'] (letter | digit |'_')* as name	{ T_name name }
  | ['A'-'Z'] (letter | digit | '_')* as name	{ T_constrname name }

  | "->"	{ T_arrow }
  | "("      	{ T_lparen }
  | ")"      	{ T_rparen }
  | "+"      	{ T_plus }
  | "-"      	{ T_minus }
  | "*"      	{ T_times }
  | "="		{ T_structeq }
  | "|"		{ T_vertbar }
  | "/"		{ T_div }
  | "+."	{ T_fplus }
  | "-."	{ T_fminus }
  | "*."	{ T_ftimes }
  | "/."	{ T_fdiv }
  | "**"	{ T_fpower }
  | "!"		{ T_dembanger }
  | ";"		{ T_semicolumn }
  | "&&"	{ T_and }
  | "||"	{ T_or }
  | "<>"	{ T_structneq }
  | "<"		{ T_ls }
  | ">"		{ T_gr }
  | "<="	{ T_leq }
  | ">="	{ T_geq }
  | "=="	{ T_naturaleq}
  | "!="	{ T_naturalneq}
  | ":="	{ T_assign }
  | "["		{ T_lbracket }
  | "]"		{ T_rbracket }
  | ","		{ T_comma }
  | ":"		{ T_colon }

  | white+              { lexer lexbuf }
  | "--" [^'\n']*	{ lexer lexbuf }
  | "(*"		{ comments 0 lexbuf }
  | '\n'		{ incr_linenum lexbuf; lexer lexbuf }

  |  eof		{ T_eof }
  |  _ as chr		{
				let pos = lexbuf.Lexing.lex_curr_p in
					Error.fatal "invalid character: '%c' (ascii: %d) at line : %d, position: %d"
						chr (Char.code chr) pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
				lexer lexbuf
			}

and comments level = parse
  | "*)" 	{ 	
			if level = 0 then lexer lexbuf
			else comments (level-1) lexbuf
  		}
  | "(*" 	{ comments (level+1) lexbuf }
  | '\n'	{ incr_linenum lexbuf ; comments level lexbuf }
  | _ 		{ comments level lexbuf }
  | eof 	{ Error.fatal "comments are not closed"; T_eof }

and chars = parse
  | '\\' 'x' hex hex '\'' as chr	{ T_cconst (getChar (List.nth (explode chr) 2) (List.nth (explode chr) 3)) }
  | '\\' 'n' '\''	 		{ T_cconst ('\n') } 
  | '\\'  't' '\''	 		{ T_cconst ('\t') } 
  | '\\' 'r' '\''	 		{ T_cconst ('\r') }
  | '\\' '0' '\''	 		{ T_cconst ('\000') }
  | '\\' '\\' '\''	 		{ T_cconst ('\\') }
  | '\\' '\'' '\''	 		{ T_cconst ('\'') }
  | '\\' '"' '\''			{ T_cconst ('"') }
  | _ '\'' as chr 			{ T_cconst (List.hd (explode chr)) }
  | _ 					{ let pos = lexbuf.Lexing.lex_curr_p in
                                          Error.fatal "invalid character at line : %d, position: %d"
                                          	pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
                                 	  lexer lexbuf
					}
  | eof					{ Error.fatal "Unterminated character at line: %d."
                          			lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum;
                        		  T_eof
					}

and str acc = parse
  | '"'					{ T_sconst (implode (List.rev acc)) }
  | '\n'				{ Error.fatal "Multiply line string starting at line: %d."
                				lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum;
                  			  T_eof
					}
  | '\\' 'n'				{ str ('\n' :: acc) lexbuf }
  | '\\' 't'				{ str ('\t' :: acc) lexbuf }
  | '\\' 'r'				{ str ('\r' :: acc) lexbuf }
  | '\\' '0'				{ str ('\000' :: acc) lexbuf } 
  | '\\' '\\'				{ str ('\\' :: acc) lexbuf }
  | '\\' '\''				{ str ('\'' :: acc) lexbuf }
  | '\\' '"'				{ str ('"' :: acc) lexbuf }
  | '\\' 'x' hex hex as chr		{ str ((getChar (List.nth (explode chr) 2) (List.nth (explode chr) 3)) :: acc) lexbuf }
  | eof					{ Error.fatal "Unterminated string at line: %d."
						lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum;
					  T_eof
					}
  | _ as chr				{ str (chr :: acc) lexbuf }




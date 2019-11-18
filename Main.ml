open Lexer
open Parser
open Constraints
open Typeinfer
open Semantics
open GenerateQuads
open Print
open GenerateAsm
open Arg
open Sys


let in_file = ref ""


let asm_out_file = ref "a.asm"


let qua_out_file = ref "a.qua"


let qua_flag = ref false


let opt_flag = ref false


let speclist = Arg.align [
	("-gi", Set qua_flag, " Generate intermediate code");
	("-O", Set opt_flag, " Enable optimizations");
	("-o", String (fun str -> asm_out_file := str; qua_out_file := str), "file Output file")
]


let anon_fun str = in_file := str


let usage_msg = "Usage: llamac [options] file...\n\nOptions:"


let main =
	Arg.parse speclist anon_fun usage_msg;
	if (not (Sys.file_exists !in_file)) then (
		Arg.usage speclist usage_msg;
		exit 1
	);
	let in_channel = open_in !in_file in
	let lexbuf = Lexing.from_channel in_channel in
	try (
		let ast = Parser.program Lexer.lexer lexbuf in
		close_in in_channel;
		let c = Constraints.generateConstraints ast in
		let typedAst = Typeinfer.typeinfer ast c in
		Semantics.check typedAst;
		let qdsListList = GenerateQuads.generateQuads typedAst in
		match (!opt_flag, !qua_flag) with
		| (false, false) ->
			let asmList = GenerateAsm.generateAsm qdsListList in
			let out_channel = open_out !asm_out_file in
			Print.printAsm (Format.formatter_of_out_channel out_channel) asmList;
			close_out out_channel;
			exit 0
		| (false, true) ->
			let out_channel = open_out !qua_out_file in
			Print.printQuads (Format.formatter_of_out_channel out_channel) qdsListList;
			close_out out_channel;
			exit 0
		| (true, false) ->
			let optimizedQdsListList = Optimizations.optimize qdsListList in
			let asmList = GenerateAsm.generateAsm optimizedQdsListList in
			let out_channel = open_out !asm_out_file in
			Print.printAsm (Format.formatter_of_out_channel out_channel) asmList;
			close_out out_channel;
			exit 0
		| (true, true) ->
			let optimizedQdsListList = Optimizations.optimize qdsListList in
			let out_channel = open_out !qua_out_file in
			Print.printQuads (Format.formatter_of_out_channel out_channel) optimizedQdsListList;
			close_out out_channel;
			exit 0
	) with
	| Parsing.Parse_error ->
		let pos = lexbuf.Lexing.lex_curr_p in
		Printf.eprintf "Syntax error at line: %d, position: %d\n" 
			pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
		close_in in_channel;
    		exit 1
	| Error.Terminate ->
		close_in in_channel;
		exit 1



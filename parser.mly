%{
	open Printf
	open Spreadsheet

	let parse_error s = 
  	print_endline s;
  	flush stdout
  let compile_index s=	(* this take input string [a,b] where a and b are numerical representation of integers and returns (a1,b1) where a1 and b1 are integers represented by a and b *)
 		let ch=',' in
 		let t = String.index s ch in 
 		let p1= String.sub s 1 (t-1) in
 		let p2= String.sub s (t+1) ((String.length s)-t-2) in
 		let num1 = int_of_string p1 in
 		let num2 = int_of_string p2 in
 		(num1,num2);;	(* (int,int) *)


 let compile_range s=	(* this take input string [(a,b):(c,d)] where a,b,c,d are numerical representation of integers and returns ((a1,b1),(c1,d1)) where a1,b1,c1,d1 are integers represented by a,b,c,d respectively *)
 		let ch=':' in
 		let t = String.index s ch in 
 		let p1= String.sub s 1 (t-1) in
 		let p2= String.sub s (t+1) ((String.length s)-t-2) in
 		let a1 = compile_index p1 in
 		let a2 = compile_index p2 in
 		(a1,a2);; 	(*  ((int,int),(int,int))  *)





let process_unary res op rng = match op with	(* process unary functions *)
		"COUNT" ->		Spreadsheet.full_count Spreadsheet.worksheet rng res 
		|"ROWCOUNT"->	Spreadsheet.row_count Spreadsheet.worksheet rng res 
		|"COLCOUNT"->	Spreadsheet.col_count Spreadsheet.worksheet rng res 
		|"SUM"->		Spreadsheet.full_sum Spreadsheet.worksheet rng res 
		|"ROWSUM"->		Spreadsheet.row_sum Spreadsheet.worksheet rng res 
		|"COLSUM"->		Spreadsheet.col_sum Spreadsheet.worksheet rng res 
		|"AVG"->		Spreadsheet.full_avg Spreadsheet.worksheet rng res
		|"ROWAVG"->		Spreadsheet.row_avg Spreadsheet.worksheet rng res 
		|"COLAVG"->		Spreadsheet.col_avg Spreadsheet.worksheet rng res 
		|"MIN"->		Spreadsheet.full_min Spreadsheet.worksheet rng res 
		|"ROWMIN"->		Spreadsheet.row_min Spreadsheet.worksheet rng res
		|"COLMIN"->		Spreadsheet.col_min Spreadsheet.worksheet rng res
		|"MAX"->		Spreadsheet.full_max Spreadsheet.worksheet rng res
		|"ROWMAX"->		Spreadsheet.row_max Spreadsheet.worksheet rng res 
		|"COLMAX"->		Spreadsheet.col_max Spreadsheet.worksheet rng res
			;;

 let process_binary1 res op rng1 rng2 =match op with	(* process binary functions for which both operators are ranges*)
 	"ADD" ->		Spreadsheet.add_range Spreadsheet.worksheet rng1 rng2 res
 	|"SUBT"->		Spreadsheet.subt_range Spreadsheet.worksheet rng1 rng2 res
 	|"MULT"->		Spreadsheet.mult_range Spreadsheet.worksheet rng1 rng2 res
 	|"DIV"->		Spreadsheet.div_range Spreadsheet.worksheet rng1 rng2 res
 		;;


 let process_binary2 res op rng con = match op with	(* process binary functions for which  operators are range andb constant*)
 	"ADD" ->		Spreadsheet.add_const Spreadsheet.worksheet rng con res
 	|"SUBT"->		Spreadsheet.subt_const Spreadsheet.worksheet rng con res
 	|"MULT"->		Spreadsheet.mult_const Spreadsheet.worksheet rng con res
 	|"DIV"->		Spreadsheet.div_const Spreadsheet.worksheet rng con res
 		;;

  let process_binary3 res op rng ind = (* process binary functions for which  operators are range andb index*)
  		let con = Spreadsheet.get Spreadsheet.worksheet ind	in	(* get is the function which returns the value at that index in that Spreadsheet.worksheet(returned value is float) get:Spreadsheet.worksheet->ind->float  *)
  	match op with
 	"ADD" ->		Spreadsheet.add_const Spreadsheet.worksheet rng con res
 	|"SUBT"->		Spreadsheet.subt_const Spreadsheet.worksheet rng con res
 	|"MULT"->		Spreadsheet.mult_const Spreadsheet.worksheet rng con res
 	|"DIV"->		Spreadsheet.div_const Spreadsheet.worksheet rng con res
 	;;

%}

%token <float> CONST
%token <char> LPARN RPARN LBRAC RBRAC COMMA COLON FOT
%token <string> INDEX RANGE BOP UOP AOP
%start line
%type <unit> line		
%%
line:
	| formula FOT	{$1}
	;
formula:INDEX AOP UOP RANGE		{ let s1 = process_unary (compile_index $1) ($3) (compile_range $4) in let k= Spreadsheet.print_sheet Spreadsheet.worksheet 0 0 10 10 in ()}							/* I := FUNC  R */
	|INDEX AOP BOP RANGE RANGE	{ let s1 = process_binary1 (compile_index $1) $3 (compile_range $4) (compile_range $5) in let k= Spreadsheet.print_sheet Spreadsheet.worksheet 0 0 10 10 in ()}		/* I := FUNC R R */
	|INDEX AOP BOP RANGE CONST 	{let s1 = process_binary2 (compile_index $1) $3 (compile_range $4) $5 in let k= Spreadsheet.print_sheet Spreadsheet.worksheet 0 0 10 10 in ()}						/* I := FUNC  R C */
	|INDEX AOP BOP CONST RANGE 	{let s1 = process_binary2 (compile_index $1) $3 (compile_range $5) $4 in let k= Spreadsheet.print_sheet Spreadsheet.worksheet 0 0 10 10 in ()}						/* I := FUNC  C R */
	|INDEX AOP BOP INDEX RANGE 	{let s1 = process_binary3 (compile_index $1) $3 (compile_range $5) (compile_index $4) in let k= Spreadsheet.print_sheet Spreadsheet.worksheet 0 0 10 10 in ()}		/* I := FUNC  I R */
	|INDEX AOP BOP RANGE INDEX 	{let s1 = process_binary3 (compile_index $1) $3 (compile_range $4) (compile_index $5) in let k= Spreadsheet.print_sheet Spreadsheet.worksheet 0 0 10 10 in ()}		/* I := FUNC  R I */
	;	
/* sheet is printed after every instruction */
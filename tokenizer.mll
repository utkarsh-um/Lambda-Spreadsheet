{
	open Parser
	open Printf


}

let digit=['0'-'9']
let nonze=['1'-'9']
let indreg='['['0'-'9']+','['0'-'9']+']'

rule token = parse
	|('+'|'-')?('0'['0'-'9']+)('.')(['0'-'9']*) 	{printf "lexical error"; exit 0;token lexbuf}			(*non acceptable floats*)

	|('+'|'-')?(['0'-'9']*)('.')(['0'-'9']+'0') 	{printf "lexical error"; exit 0;token lexbuf}			(*non acceptable floats*)

	|('+'|'-')?('0'|['1'-'9']['0'-'9']*)('.')(['0'-'9']*['1'-'9']|'0') as fcons
		{
			let num = float_of_string fcons in																(*acceptable floats*)
			CONST num
		}

	|'(' as lp
		{
			
			LPARN lp 																	(*left paranthesis*)
						
		}

	|')' as rp
		{
			
			RPARN rp 																	(*right paranthesis*)			
			
		}

	|'[' as lb
		{
			
			LBRAC lb 																	(*left bracket*)
			
		}

	|']' as rb
		{
			
			RBRAC rb 																	(*right bracket*)
			
		}

	|',' as cm
		{
			 
			COMMA cm 																	(*comma*)
			
		}

	|':' as cl
		{
			
			COLON cl 																	(*colon*)
			
		}

	| indreg as ind
		{
			INDEX ind 																	(*index*)
			
		}

	| '('(indreg)':'(indreg)')' as ran
		{
			
			RANGE ran 																	(* Ranges*)
			
		}

	|"COUNT"
	|"ROWCOUNT"
	|"COLCOUNT"
	|"SUM"
	|"ROWSUM"
	|"COLSUM"
	|"AVG"
	|"ROWAVG"
	|"COLAVG"
	|"MIN"
	|"ROWMIN"
	|"COLMIN"
	|"MAX"
	|"ROWMAX"
	|"COLMAX"	as unaop
		{
			
			UOP unaop 																	(*unary ops*)
			
		}

	|"ADD"
	|"SUBT"
	|"MULT"
	|"DIV"	as binop
		{
			
			BOP binop 																	(*binary ops*)
			
		}

	|":=" as asop
		{
			AOP asop 																	(*assignment ops*)
			
		}

	|';' as fort
		{
			FOT fort 																	(*semicolon*)
			
		}
	|['\n' ' ' '\t']+	{token lexbuf}
	|_ as c 	{printf "lexical error"; exit 0;token lexbuf}
	| eof 	 {raise End_of_file}





















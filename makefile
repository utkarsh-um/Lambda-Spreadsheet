default : run	
run: 
	ocamlc -c spreadsheet.ml
	ocamllex tokenizer.mll
	ocamlyacc parser.mly     
	ocamlc -c parser.mli
	ocamlc -c tokenizer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o assignment4 str.cma tokenizer.cmo spreadsheet.cmo parser.cmo main.cmo 	
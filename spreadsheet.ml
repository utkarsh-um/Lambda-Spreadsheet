module Spreadsheet =
   struct
   exception EMPTYCELLS;;
   	 exception UNEQUALRANGE;;
     type index=int*int;;
     type range=(int*int)*(int*int);;
     type cell=Empty|Val of float;;									(* type of cell of the sheet *)
     type sheet=cell array array;;									(* sheet is a matrix/2D array of type cell *)
     let (worksheet:sheet) = Array.make_matrix 5000 5000 (Empty);; (* sheet with dimensions 5000X5000 used in parser and main *)
     let rec reverse v rev = match v with
					[]->rev
					| x::xs -> (reverse xs (x::rev));;

     let get_dimensions (s:sheet)=(Array.length s,Array.length (Array.get s 0));;	(* returns the dimensions of the sheet *)

     let get (s:sheet) (i:index) = 													(* returns the element at ith index of sheet *)
     	let (a1,a2) = i in
     		match s.(a1).(a2) with
     		|Empty -> raise EMPTYCELLS
     		|Val(f) -> f;;



     let rec print_sheet s i j m n= if i=m then let lk=Printf.printf "---------------------------------------\n"; flush stdout; in ()  (* function to print the sheet here m n are the arguments that define the dimensions to be printed *)
        else if j=n then let lk=Printf.printf "\n"; flush stdout; in print_sheet s (i+1) 0 m n 
        else match s.(i).(j) with
        | Empty ->  Printf.printf "Empty "; flush stdout;  print_sheet s i (j+1) m n
          |Val(t) ->Printf.printf "%f " t; flush stdout; print_sheet s i (j+1) m n;;
(*------------------------------------------------------------------------------------------------------------------------------------------*)

	let rec full_count_help s i j m1 n1 m2 n2 c = if i=(m2+1) then c 			(* helper function for full_count traverses the sheet  *)
			  else if j=(n2+1) then full_count_help s (i+1) n1 m1 n1 m2 n2 c   	(* row by row *)
			  else match s.(i).(j) with
			  | Empty -> full_count_help s i (j+1) m1 n1 m2 n2 c 				(* counts only non Empty entries*)
				  | _ -> full_count_help s i (j+1) m1 n1 m2 n2 (c+1);; 			(* count stored in c*)

     let full_count (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let count = full_count_help s a1 a2 a1 a2 b1 b2 0 in
     		let (s1,s2)=i in
     		let ik = s.(s1).(s2)<-Val(float_of_int(count)) in
     		s;;
(*------------------------------------------------------------------------------------------------------------------------------------------*)

	let rec row_count_help s i j m1 n1 m2 n2 c anli = if i=(m2+1) then (reverse (anli) []) 			(* helper function for row_count traverses the sheet   *)
			else if j=(n2+1) then row_count_help s (i+1) n1 m1 n1 m2 n2 0 (c::anli) 				(* row by row and result stored in a list and the end of row count set to 0*)
			else match s.(i).(j) with
			| Empty -> row_count_help s i (j+1) m1 n1 m2 n2 c anli 									(* counts only non Empty entries*)
			| _ -> row_count_help s i (j+1) m1 n1 m2 n2 (c+1) anli;; 								(* count of current row stored in c*)

	let rec row_counthelp1 s i j anli = match anli with
	[]->() 																							(* fills the data at required place*)
	|x::xs-> let m=(s.(i).(j)<-Val(float_of_int(x))) in row_counthelp1 s (i+1) j xs;;

     let row_count (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let (s1,s2)=i in
     		let anli=row_count_help s a1 a2 a1 a2 b1 b2 0 [] in
     		let ik = row_counthelp1 s s1 s2 anli in
     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
			let rec col_count_help s i j m1 n1 m2 n2 c anli = if j=(n2+1) then (reverse (anli) [])    	(* helper function for col_count traverses the sheet   *)
			else if i=(m2+1) then col_count_help s m1 (j+1) m1 n1 m2 n2 0 (c::anli)  					(* col by col and result stored in a list and the end of a col, count set to 0*)
			else match s.(i).(j) with
			| Empty -> col_count_help s (i+1) j m1 n1 m2 n2 c anli 										(* counts only non Empty entries*)
			| _ -> col_count_help s (i+1) j m1 n1 m2 n2 (c+1) anli;; 									(* count of current col stored in c*)

			let rec col_counthelp1 s i j anli = match anli with 										(* fills the data at required place*)
			[]->()
			|x::xs-> let m=(s.(i).(j)<-Val(float_of_int(x))) in col_counthelp1 s i (j+1) xs;;
	
			let col_count (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let (s1,s2)=i in
     		let anli=col_count_help s a1 a2 a1 a2 b1 b2 0 [] in
     		let ik=col_counthelp1 s s1 s2 anli in 
     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
		
		let rec full_sum_help s i j m1 n1 m2 n2 c = if i=(m2+1) then c 					(* helper function for full_sum traverses the sheet   *)
			  else if j=(n2+1) then full_sum_help s (i+1) n1 m1 n1 m2 n2 c  			(* row by row *)
			  else match s.(i).(j) with
			  | Empty -> raise EMPTYCELLS 												(* Exception in case of Empty Cell*)
			| Val(f) -> full_sum_help s i (j+1) m1 n1 m2 n2 (c+.f);; 					(*sum stored in c*)

		let full_sum (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let sum = full_sum_help s a1 a2 a1 a2 b1 b2 0.0 in
     		let (s1,s2)=i in
     		let ik = s.(s1).(s2)<-Val(sum) in
     		s;;		
(*------------------------------------------------------------------------------------------------------------------------------------------*)
			
			let rec row_sum_help s i j m1 n1 m2 n2 c anli = if i=(m2+1) then (reverse (anli) []) 		(* helper function for row_sum traverses the sheet   *)
			else if j=(n2+1) then row_sum_help s (i+1) n1 m1 n1 m2 n2 0.0 (c::anli)  					(* row by row and result stored in a list and the end of each row,  sum set to 0.0*)
			else match s.(i).(j) with
			| Empty -> raise EMPTYCELLS 																(* Exception in case of Empty Cell*)
			| Val(f)-> row_sum_help s i (j+1) m1 n1 m2 n2 (c+.f) anli;; 								(* sum of current row stored in c*)

			let rec row_sumhelp1 s i j anli = match anli with 											(* fills the data at required place*)
			[]->()
			|x::xs-> let m=(s.(i).(j)<-Val(x)) in row_sumhelp1 s (i+1) j xs;;

		     let row_sum (s:sheet) (r:range) (i:index) = 
		     		let ((a1,a2),(b1,b2))=r in
		     		let (s1,s2)=i in
		     		let anli=row_sum_help s a1 a2 a1 a2 b1 b2 0.0 [] in
		     		let ik = row_sumhelp1 s s1 s2 anli in
		     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
			let rec col_sum_help s i j m1 n1 m2 n2 c anli = if j=(n2+1) then (reverse (anli) [])    	(* helper function for col_sum traverses the sheet   *)
			else if i=(m2+1) then col_sum_help s m1 (j+1) m1 n1 m2 n2 0.0 (c::anli)  					(* col by col and result stored in a list and the end of a col, sum set to 0.0*)
			else match s.(i).(j) with
			| Empty -> raise EMPTYCELLS 																(* Exception in case of Empty Cell*)
			| Val(f) -> col_sum_help s (i+1) j m1 n1 m2 n2 (c+.f) anli;; 								(* sum of current col stored in c*)

			let rec col_sumhelp1 s i j anli = match anli with 											(* fills the data at required place*)
			[]->()
			|x::xs-> let m=(s.(i).(j)<-Val(x)) in col_sumhelp1 s i (j+1) xs;;
	
			let col_sum (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let (s1,s2)=i in
     		let anli=col_sum_help s a1 a2 a1 a2 b1 b2 0.0 [] in
     		let ik = col_sumhelp1 s s1 s2 anli in
     		s;;
	

(*------------------------------------------------------------------------------------------------------------------------------------------*)

			(*for all Average functions corresponding sum/corresponding count *)
			(*Exception raised if empty cell*)

			let full_avg (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let sum = full_sum_help s a1 a2 a1 a2 b1 b2 0.0 in
     		let count = full_count_help s a1 a2 a1 a2 b1 b2 0 in
     		let (s1,s2)=i in
	       let ik= 		s.(s1).(s2)<-Val(sum/.(float_of_int(count))) in
	   		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
			let rec row_avghelp1 s i j sumli countli= match (sumli,countli) with
			([],[])->()
			|(sum::ss,count::cs)-> let m=(s.(i).(j)<-Val(sum/.(float_of_int(count)))) in row_avghelp1 s (i+1) j ss cs;;

 			let row_avg (s:sheet) (r:range) (i:index) = 
		     		let ((a1,a2),(b1,b2))=r in
		     		let (s1,s2)=i in
		     		let sumli=row_sum_help s a1 a2 a1 a2 b1 b2 0.0 [] in
		     		let countli=row_count_help s a1 a2 a1 a2 b1 b2 0 [] in
		     		let ik=row_avghelp1 s s1 s2 sumli countli in
		     			s;;
(*------------------------------------------------------------------------------------------------------------------------------------------*)


				let rec col_avghelp1 s i j sumli countli= match (sumli,countli) with
			([],[])->()
			|(sum::ss,count::cs)-> let m=(s.(i).(j)<-Val(sum/.(float_of_int(count)))) in col_avghelp1 s i (j+1) ss cs;;


				let col_avg (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let (s1,s2)=i in
     		let sumli=col_sum_help s a1 a2 a1 a2 b1 b2 0.0 [] in
     		let countli=col_count_help s a1 a2 a1 a2 b1 b2 0 [] in
     		let ik = col_avghelp1 s s1 s2 sumli countli in
     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
		
		let rec full_min_help s i j m1 n1 m2 n2 c = if i=(m2+1) then c    				(* helper function for full_min traverses the sheet   *)
			  else if j=(n2+1) then full_min_help s (i+1) n1 m1 n1 m2 n2 c    			(* row by row *)
			  else match s.(i).(j) with
			  | Empty -> raise EMPTYCELLS 												(* Exception in case of Empty Cell*)
			| Val(f) -> if f<c then full_min_help s i (j+1) m1 n1 m2 n2 (f) 			(* min stored in c *)
						else full_min_help s i (j+1) m1 n1 m2 n2 (c);;

		let full_min (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let m = match s.(a1).(a2) with
     			  	| Empty -> raise EMPTYCELLS
					| Val(f) ->f
     		in
     		let min = full_min_help s a1 a2 a1 a2 b1 b2 m in
     		let (s1,s2)=i in
     		let ik = s.(s1).(s2)<-Val(min) in
     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)

			let rec row_min_help s i j m1 n1 m2 n2 c anli = if i=(m2) && j=(n2+1) then (reverse (c::anli) [])   (* helper function for row_min traverses the sheet   *)
			else if j=(n2+1) && i<>m2 then  											(* row by row and result stored in a list and at the end of each row,  min set to the first value of next row*)
						let m = match s.(i+1).(n1) with
	     		  			| Empty -> raise EMPTYCELLS 								(* Exception in case of Empty Cell*)
							| Val(f) ->f 												(* min of current row stored in c*)
     					in
					row_min_help s (i+1) n1 m1 n1 m2 n2 m (c::anli) 
			else match s.(i).(j) with
			| Empty -> raise EMPTYCELLS
			| Val(f)-> if f<c then row_min_help s i (j+1) m1 n1 m2 n2 (f) anli
						else row_min_help s i (j+1) m1 n1 m2 n2 (c) anli;;



			let rec row_minhelp1 s i j anli = match anli with
			[]->()
			|x::xs-> let m=(s.(i).(j)<-Val(x)) in row_minhelp1 s (i+1) j xs;;



		     let row_min (s:sheet) (r:range) (i:index) = 
		     		let ((a1,a2),(b1,b2))=r in
		     		let (s1,s2)=i in
		     		let m = match s.(a1).(a2) with
		     			  	| Empty -> raise EMPTYCELLS
							| Val(f) ->f
		     		in
     		let anli=row_min_help s a1 a2 a1 a2 b1 b2 m [] in
     		let ik = row_minhelp1 s s1 s2 anli in
     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
			let rec col_min_help s i j m1 n1 m2 n2 c anli = if j=(n2) && (i=m2+1) then (reverse (c::anli) [])  	(* helper function for col_min traverses the sheet   *)
			else if i=(m2+1) && j<>n2 then 										(* col by col and result stored in a list and at the end of a col, min set to he first value of next col*)
							let m = match s.(m1).(j+1) with
	     		  			| Empty -> raise EMPTYCELLS
							| Val(f) ->f
     						in
						 col_min_help s m1 (j+1) m1 n1 m2 n2 m (c::anli) 
			else match s.(i).(j) with 											(* Exception in case of Empty Cell*)
			| Empty -> raise EMPTYCELLS
			| Val(f) -> if f<c then col_min_help s (i+1) j m1 n1 m2 n2 (f) anli 		(* min of current col stored in c*)
						else col_min_help s (i+1) j m1 n1 m2 n2 (c) anli;;

			let rec col_minhelp1 s i j anli = match anli with
			[]->()
			|x::xs-> let m=(s.(i).(j)<-Val(x)) in col_minhelp1 s i (j+1) xs;;
	
			let col_min (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let (s1,s2)=i in
     		let m = match s.(a1).(a2) with
		     			  	| Empty -> raise EMPTYCELLS
							| Val(f) ->f
		     		in
     		let anli=col_min_help s a1 a2 a1 a2 b1 b2 m [] in
     		let ik = col_minhelp1 s s1 s2 anli in
     			s;;
	
(*------------------------------------------------------------------------------------------------------------------------------------------*)

		(*max functions are similar to min except for the less than is changed with greater than*)

	let rec full_max_help s i j m1 n1 m2 n2 c = if i=(m2+1) then c
			  else if j=(n2+1) then full_max_help s (i+1) n1 m1 n1 m2 n2 c 
			  else match s.(i).(j) with
			  | Empty -> raise EMPTYCELLS
			| Val(f) -> if f>c then full_max_help s i (j+1) m1 n1 m2 n2 (f)
						else full_max_help s i (j+1) m1 n1 m2 n2 (c);;

		let full_max (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let m = match s.(a1).(a2) with
     		  | Empty -> raise EMPTYCELLS
			| Val(f) ->f
     	in
     		let max = full_max_help s a1 a2 a1 a2 b1 b2 m in
     		let (s1,s2)=i in
     		let ik = s.(s1).(s2)<-Val(max) in
     		s;;	


(*------------------------------------------------------------------------------------------------------------------------------------------*)

 let rec row_max_help s i j m1 n1 m2 n2 c anli = if i=(m2) && j=(n2+1) then (reverse (c::anli) [])
			else if j=(n2+1) && i<>m2 then 
						let m = match s.(i+1).(n1) with
	     		  			| Empty -> raise EMPTYCELLS
							| Val(f) ->f
     					in
					row_max_help s (i+1) n1 m1 n1 m2 n2 m (c::anli) 
			else match s.(i).(j) with
			| Empty -> raise EMPTYCELLS
			| Val(f)-> if f>c then row_max_help s i (j+1) m1 n1 m2 n2 (f) anli
						else row_max_help s i (j+1) m1 n1 m2 n2 (c) anli;;



			let rec row_maxhelp1 s i j anli = match anli with
			[]->()
			|x::xs-> let m=(s.(i).(j)<-Val(x)) in row_maxhelp1 s (i+1) j xs;;



		     let row_max (s:sheet) (r:range) (i:index) = 
		     		let ((a1,a2),(b1,b2))=r in
		     		let (s1,s2)=i in
		     		let m = match s.(a1).(a2) with
		     			  	| Empty -> raise EMPTYCELLS
							| Val(f) ->f
		     		in
     		let anli=row_max_help s a1 a2 a1 a2 b1 b2 m [] in
     		let ik = row_maxhelp1 s s1 s2 anli in 
     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)

     		let rec col_max_help s i j m1 n1 m2 n2 c anli = if j=(n2) && (i=m2+1) then (reverse (c::anli) [])
			else if i=(m2+1) && j<>n2 then
							let m = match s.(m1).(j+1) with
	     		  			| Empty -> raise EMPTYCELLS
							| Val(f) ->f
     						in
						 col_max_help s m1 (j+1) m1 n1 m2 n2 m (c::anli) 
			else match s.(i).(j) with
			| Empty -> raise EMPTYCELLS
			| Val(f) -> if f>c then col_max_help s (i+1) j m1 n1 m2 n2 (f) anli
						else col_max_help s (i+1) j m1 n1 m2 n2 (c) anli;;

			let rec col_maxhelp1 s i j anli = match anli with
			[]->()
			|x::xs-> let m=(s.(i).(j)<-Val(x)) in col_maxhelp1 s i (j+1) xs;;
	
			let col_max (s:sheet) (r:range) (i:index) = 
     		let ((a1,a2),(b1,b2))=r in
     		let (s1,s2)=i in
     		let m = match s.(a1).(a2) with
		     			  	| Empty -> raise EMPTYCELLS
							| Val(f) ->f
		     		in
     		let anli=col_max_help s a1 a2 a1 a2 b1 b2 m [] in
     		let ik =col_maxhelp1 s s1 s2 anli in 
     		s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
		
		(* for add_const subt_const mult _const div_const only one common helper function arth_const_help which takes input a operator which is +. -. *. /. respectively*)
		(* a 2D array of same size as that of range is created in which values generated after operation are stored once and later on these values are copied to the required cells in sheet*)
		(* using arth_consthelp1*)

				let rec arth_const_help s i j m1 n1 m2 n2 a c op= if i=(m2+1) then ()
			  	else if j=(n2+1) then arth_const_help s (i+1) n1 m1 n1 m2 n2 a c op
			  	else match s.(i).(j) with
			  	| Empty -> raise EMPTYCELLS
				  | Val(f) -> let x = (a.(i-m1).(j-n1)<-(op f c)) in arth_const_help s i (j+1) m1 n1 m2 n2 a c op;;


				let rec arth_consthelp1 s i1 j1 m1 n1 a i2 j2 m2 n2 = if i2=(m2+1) then ()
			  	else if j2=(n2+1) then arth_consthelp1 s (i1+1) n1 m1 n1 a (i2+1) 0 m2 n2
			  	else	let xyz = (s.(i1).(j1)<-(Val(a.(i2).(j2)))) in arth_consthelp1 s i1 (j1+1) m1 n1 a i2 (j2+1) m2 n2;;
			  	


				let add_const (s:sheet) (r:range) (c:float) (i:index) = 
				let ((a1,a2),(b1,b2))=r in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b1-a1+1) (b2-a2+1) 0.0 in
     			let op x y=(x+.y) in
     			let m = arth_const_help s a1 a2 a1 a2 b1 b2 a c op in 
     			let ik = arth_consthelp1 s s1 s2 s1 s2 a 0 0 (b1-a1) (b2-a2) in
     			s;;

     			let subt_const (s:sheet) (r:range) (c:float) (i:index) = 
				let ((a1,a2),(b1,b2))=r in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b1-a1+1) (b2-a2+1) 0.0 in
     			let op x y=(x-.y) in
     			let m = arth_const_help s a1 a2 a1 a2 b1 b2 a c op in 
     			let ik =  arth_consthelp1 s s1 s2 s1 s2 a 0 0 (b1-a1) (b2-a2) in
     			s;;

     			let mult_const (s:sheet) (r:range) (c:float) (i:index) = 
				let ((a1,a2),(b1,b2))=r in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b1-a1+1) (b2-a2+1) 0.0 in
     			let op x y=(x*.y) in
     			let m = arth_const_help s a1 a2 a1 a2 b1 b2 a c op in 
     			let ik =arth_consthelp1 s s1 s2 s1 s2 a 0 0 (b1-a1) (b2-a2) in
     			s;;

     			let div_const (s:sheet) (r:range) (c:float) (i:index) = 
				let ((a1,a2),(b1,b2))=r in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b1-a1+1) (b2-a2+1) 0.0 in
     			let op x y=(x/.y) in
     			let m = arth_const_help s a1 a2 a1 a2 b1 b2 a c op in 
     			let ik = arth_consthelp1 s s1 s2 s1 s2 a 0 0 (b1-a1) (b2-a2) in
     			s;;

(*------------------------------------------------------------------------------------------------------------------------------------------*)
		(*check for equality of dimensions of ranges*)
     			let check_ranges a11 a12 b11 b12 a21 a22 b21 b22 = if (b11-a11,b12-a12) = (b21-a21,b22-a22) then true else raise UNEQUALRANGE;;

(* for add_const subt_const mult _const div_const only one common helper function arth_range_help which takes input a operator which is +. -. *. /. respectively*)
(* a 2D array of same size as that of range is created in which values generated after operation are stored once and later on these values are copied to the required cells in sheet*)
(* using arth_consthelp1*)

     			let rec arth_range_help s i1 j1 m11 n11 m12 n12 i2 j2 m21 n21 m22 n22 a op = if i1=(m12+1) then ()
			  	else if j1=(n12+1) then arth_range_help s (i1+1) n11 m11 n11 m12 n12 (i2+1) n21 m21 n21 m22 n22 a op
			  	else match (s.(i1).(j1),s.(i2).(j2)) with
			  	| (Empty,Empty) -> raise EMPTYCELLS
			  	| (Val(f),Empty) -> raise EMPTYCELLS
			  	| (Empty,Val(f)) -> raise EMPTYCELLS
				  | (Val(f1),Val(f2)) -> let x = (a.(i1-m11).(j1-n11)<-(op f1 f2)) in arth_range_help s (i1) (j1+1) m11 n11 m12 n12 (i2) (j2+1) m21 n21 m22 n22 a op;;

     			let add_range (s:sheet) (r1:range) (r2:range) (i:index) = 
     			let ((a11,a12),(b11,b12))=r1 in
     			let ((a21,a22),(b21,b22))=r2 in
     			let xyz=check_ranges a11 a12 b11 b12 a21 a22 b21 b22 in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b11-a11+1) (b12-a12+1) 0.0 in
     			let op x y=(x+.y) in
     			let m = arth_range_help s a11 a12 a11 a12 b11 b12 a21 a22 a21 a22 b21 b22 a op in 
     			let ik =arth_consthelp1 s s1 s2  s1 s2 a 0 0 (b11-a11) (b12-a12) in
     			s;;


     			let subt_range (s:sheet) (r1:range) (r2:range) (i:index) = 
     			let ((a11,a12),(b11,b12))=r1 in
     			let ((a21,a22),(b21,b22))=r2 in
     			let xyz=check_ranges a11 a12 b11 b12 a21 a22 b21 b22 in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b11-a11+1) (b12-a12+1) 0.0 in
     			let op x y=(x-.y) in
     			let m = arth_range_help s a11 a12 a11 a12 b11 b12 a21 a22 a21 a22 b21 b22 a op in 
     			let ik =arth_consthelp1 s s1 s2  s1 s2 a 0 0 (b11-a11) (b12-a12) in
     			s;;


     			let mult_range (s:sheet) (r1:range) (r2:range) (i:index) = 
     			let ((a11,a12),(b11,b12))=r1 in
     			let ((a21,a22),(b21,b22))=r2 in
     			let xyz=check_ranges a11 a12 b11 b12 a21 a22 b21 b22 in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b11-a11+1) (b12-a12+1) 0.0 in
     			let op x y=(x*.y) in
     			let m = arth_range_help s a11 a12 a11 a12 b11 b12 a21 a22 a21 a22 b21 b22 a op in 
     			let ik = arth_consthelp1 s s1 s2  s1 s2 a 0 0 (b11-a11) (b12-a12) in
     			s;;


     			let div_range (s:sheet) (r1:range) (r2:range) (i:index) = 
     			let ((a11,a12),(b11,b12))=r1 in
     			let ((a21,a22),(b21,b22))=r2 in
     			let xyz=check_ranges a11 a12 b11 b12 a21 a22 b21 b22 in
     			let (s1,s2)=i in
     			let a = Array.make_matrix (b11-a11+1) (b12-a12+1) 0.0 in
     			let op x y=(x/.y) in
     			let m = arth_range_help s a11 a12 a11 a12 b11 b12 a21 a22 a21 a22 b21 b22 a op in 
     			let ik=arth_consthelp1 s s1 s2  s1 s2 a 0 0 (b11-a11) (b12-a12) in
     			s;;
     
   end;;

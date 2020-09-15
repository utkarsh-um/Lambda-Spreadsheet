open Printf;;
open Spreadsheet;;
open Str;;





let m = int_of_string(Sys.argv.(2));;
let n= int_of_string(Sys.argv.(3));;


let rec load_values s i j v = match v with
      []->()
  |   x::xs -> match x with 
      "" -> load_values s i (j+1) xs
      |_->let xyz = (s.(i).(j)<-(Spreadsheet.Val(float_of_string(x)))) in load_values s i (j+1) xs;;
  



let load_csv = 
  try
    let in_stream = open_in (Sys.argv.(1)) in
        for i=0 to (m-1) do
          let line = input_line in_stream in
          let split = Str.split_delim (Str.regexp ",") in
          let values = split line in
            load_values Spreadsheet.worksheet i 0 values;
        done;
        close_in in_stream; 
        Spreadsheet.print_sheet Spreadsheet.worksheet 0 0 10 10;
        Printf.printf "Sheet Filled \n --------------------------------------------------------\n";
  with e ->
    Printf.printf "Error while reading CSV!";
    raise e


  
        let parser_work =
          try

            let lexbuf = Lexing.from_channel stdin in
            let cmnds=[] in
            while true do
              let result = Parser.line Tokenizer.token lexbuf in
              let a=result in
                flush stdout
            done
          with End_of_file -> exit 0;;
            

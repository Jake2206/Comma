open Printf
open Csv
(* Needed: matrix to (string list list) conversion,
(string list list) conversion to matrix

Final functions:
let parseCSV fileRoute = to_int_matrix (read_csv fileRoute)
let compareCSV csv1 csv2 = Csv.compare (to_string matrix csv1) (to_string matrix csv2)
let outputCSV fname csv = 
    let pname = Filename.concat (Filename.get_temp_dir_name()) fname in
      Csv.save pname (to_string_matrix csv); *)
type matrix = int list list
(*
let to_int_matrix = 

let to_string_matrix = 
*)
let parseCSV fileRoute = Csv.load fileRoute

let compare_csv csv1 csv2 = Csv.compare csv1 csv2

let outputCSV fname csv = 
    let pname = Filename.concat (Filename.get_temp_dir_name()) fname in
      Csv.save pname csv;
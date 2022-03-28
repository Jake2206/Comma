let get_first_item_in_tuple tuple = 
  match tuple with
  | (a, _, _) -> a
  
let get_second_item_in_tuple tuple = 
  match tuple with
  | (_, a, _) -> a
    

let get_third_item_in_tuple tuple = 
  match tuple with
  | (_, _, a) -> a

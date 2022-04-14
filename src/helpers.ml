let get_first_item_in_tuple tuple = 
  match tuple with
  | (a, _) -> a
  
let get_second_item_in_tuple tuple = 
  match tuple with
  | (_, a) -> a
    
let list_to_string f lst = 
  let rec lststr f lst = 
    match lst with
    | hd::md::tl -> (f hd)^";"^(lststr f (md::tl))
    | hd::[] -> (f hd)
    | [] -> raise (Failure "waaaaah list_to_string") in
  "[" ^ (lststr f lst) ^ "]" 

let rec cpt_list_to_string cpts = 
  let cptstr cpt = 
    match cpt with
    | (rndcurr, vals) -> "("^(list_to_string (string_of_int) rndcurr)^","^(list_to_string (string_of_float) vals)^")"
    | _ -> raise (Failure "woauntoueoubtoubeoubotubrosu") in
  list_to_string (cptstr) cpts

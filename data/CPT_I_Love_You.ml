(*   [fold;call;raise;check;bet;no action]  *)

let bet hand board = 
  let x = hand-board in 
  match x<5, x<26 with
  | true,_ -> 0.10375*.(float_of_int x)
  | _,true -> (0.0234*.(float_of_int x))+.0.415
  | _ -> 1. 

let fold hand board prev = 
  let x = float_of_int (hand-board) in 
  if x < 2. then 
    match prev with 
    | 4 -> 0.9 -. 0.15*.x
    | _ -> 1. -. 0.1*.x
  else if x < 5. then
    match prev with 
    | 4 -> -0.2333*.x +. 0.9833
    | _ -> -0.2667*.x +. 1.1667
  else if x < 9. then
    match prev with 
    | 4 -> -0.0125*.x +. 0.1
    | _ -> -0.025*.x +. 0.2
  else 0.
  
let call hand board prev = 
  let x = float_of_int (hand-board) in 
  if x < 5. then 
    match prev with 
    | 4 -> (0.0125*.x*.x)+.(0.1375*.x)+.(0.1)
    | _ -> (0.0375*.x*.x)+.(0.0625*.x)
  else if x < 17. then
    match prev with 
    | 4 -> if (x < 16.) then (-0.001*.x*.x*.x)+.(0.0378*.x*.x)-.(0.4929*.x)+.(2.28) else 0.02 
    | _ -> (-0.0005*.x*.x*.x)+.(0.0219*.x*.x)-.(0.3292*.x)+.(1.85)
  else 0.

let list_prob n m u = 
  match n with 
  | 2 | 4 -> [fold m u n;call m u n;1.-.(call m u n)-.(fold m u n);0.;0.;0.]
  | 3 | 5 -> [0.;0.;0.;1.-.(bet m u);bet m u;0.]
  | _ -> [0.;0.;0.;0.;0.;1.]

let rec gen n m u l : ((int list)*(float list)) list =
  match n,m,u with 
  | 2,0,0 -> l 
  | 2,0,_ -> gen 5 43 (u-1) (([n;m;u], (list_prob n m u))::l)
  | 2,_,_ -> gen 5 (m-1) u  (([n;m;u], (list_prob n m u))::l)
  | _ -> gen (n-1) m u (([n;m;u], (list_prob n m u))::l)

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
    | (rndcurr, vals) -> "("^(list_to_string (string_of_int) rndcurr)^","^(list_to_string (string_of_float) vals)^")\n\n"
    | _ -> raise (Failure "woauntoueoubtoubeoubotubrosu") in
  list_to_string (cptstr) cpts

let () = 
  let oc = open_out "nogoodcomplicated.txt" in 
  Printf.fprintf oc "%s" (cpt_list_to_string (gen 5 43 43 []));
  close_out oc;;

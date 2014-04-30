open Bruteforce
(*
let round1 = BruteForce.get_vals 1000000 2 
let round2 = BruteForce.get_vals 1000000 5
let round3 = BruteForce.get_vals 1000000 6
let round4 = BruteForce.get_vals 1000000 7
*)
(*let final_dist = BruteForce.get_final 10000000 *)


let transform rnd vals = 
  List.mapi 
    (fun i a -> ([rnd; (i)], a)) 
    vals
    
let transform_final vals = 
  List.mapi 
    (fun i a -> ([i], a)) 
    vals    
(*
let rnd1 = transform 1 round1
let rnd2 = transform 2 round2
let rnd3 = transform 3 round3
let rnd4 = transform 4 round4 *)


(*let board1 = transform 1 (BruteForce.get_board 100000 0)
let board2 = transform 2 (BruteForce.get_board 100000 3) 
let board3 = transform 3 (BruteForce.get_board 100000 4) 
let board4 = transform 4 (BruteForce.get_board 100000 5) *)

let compare = transform_final (BruteForce.get_compare_finals 100000)

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
    | (rndcurr, vals) -> "("^(list_to_string (string_of_int) rndcurr)^","^(list_to_string (string_of_float) vals)^")\n\n" in
  list_to_string (cptstr) cpts
(*
let () =
  let oc = open_out "CPT.txt" in
  Printf.fprintf oc "%s" ((cpt_list_to_string (transform 1 round1))^"\n"^
    (cpt_list_to_string (transform 2 round2))^"\n"^(cpt_list_to_string (transform 3 round3))^"\n"^
    (cpt_list_to_string (transform 4 round4)));
  close_out oc;; *)
  
(*let () =
  let oc = open_out "BoardPT.txt" in
  Printf.fprintf oc "%s" ((cpt_list_to_string (board1))^"\n"^
    (cpt_list_to_string (board2))^"\n"^(cpt_list_to_string (board3))^"\n"^
    (cpt_list_to_string (board4)));
  close_out oc;; *)

let () =
  let oc = open_out "OppFinalPT.txt" in
  Printf.fprintf oc "%s" ((cpt_list_to_string (compare)));
  close_out oc;; 
  
let list_list_to_string lsts = 
    list_to_string (list_to_string (string_of_float)) lsts

(*let () =
  let oc = open_out "finals.txt" in
  Printf.fprintf oc "%s" ((list_list_to_string (final_dist)));
  close_out oc;; *)

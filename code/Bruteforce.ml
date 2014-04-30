open PokerEval

type card = char*int 
type deck = card list

module type BRUTEFORCE = 
sig 
  val get_vals : int -> int -> float list list
  val get_final : int -> float list list 
  val get_board : int -> int -> float list list 
  val get_compare_finals : int -> float list list 
end 

module BruteForce : BRUTEFORCE = 
struct 
  (* Generate lists of length n of repeated values*)
  let rec gen_list n k lst = 
    match n with
    | 0 -> lst
    | _ -> gen_list (n-1) k (k::lst)

  (* Generate empty 2d list consisting of 44 lists of 44 zeroes.*)
  let start_tables = gen_list 44 (gen_list 44 0 []) []
  
  (* Generate empty 1d list of 44 zeroes *)
  let start_list = gen_list 44 0 [] 

  (* Take a final/current 2d list table thingy and return it 
     with the value at Final = fin, Current = stt incremented by 1.
     Indexed starting at 1. *)
  let rec plus1 fin stt lst = 
    let rec walk_inner stt lst= 
      match stt, lst with 
      | 1, hd::tl -> (hd + 1)::tl
      | _, hd::tl -> hd::(walk_inner (stt-1) tl)
      | _, [] -> raise (Failure "index too high on update (inner)") in
    match fin, lst with 
    | 1, hd::tl -> (walk_inner stt hd)::tl
    | _, hd::tl -> hd::(plus1 (fin-1) stt tl) 
    | _, [] -> raise (Failure "index too high on update (outer)")

  (* Incrementing the list of non-conditional final distributions *)  
  let rec add1 rank lst = 
    match rank, lst with 
      | 1, hd::tl -> (hd + 1)::tl 
      | _, hd::tl -> hd::(add1 (rank-1) tl)
      | _, [] -> raise (Failure "bitch you did something wrong")

  (* Generates a list of each value at position ht of the inner lists
     of a 2d list lst. So this can be used to grab all of the hand-probs
     where the starting value is 4. Indexed starting at 1. *)
  let grab_all_starting (ht:int) (lst:'a list list) = 
    let rec grabr (ht:int) (lst:'a list list) (out:'a list) = 
      match lst with 
      | hd::tl -> grabr ht tl ((List.nth hd (ht-1))::out)
      | [] -> out in
    List.rev (grabr ht lst [])

  (* turns a 2d list of ints into a 2d list of probabilities. *)
  let tabulate (tables: int list list) : float list list =
    let ftbles = 
      List.map 
        (fun b -> List.map (fun a -> float_of_int a) b) 
        tables in
    let tablesums = 
      List.map 
        (fun a -> List.fold_left (fun n m -> n +. m) 0. a) 
        ftbles in 
    List.mapi 
      (fun i a -> 
        List.map 
  	(fun b -> 
  	  let n = (List.nth tablesums i) in 
  	  let n = if n = 0. then 1. else n in 
  	  b /. n) 
  	a) 
      ftbles


  (* Brute-forces probabilities of final hand types given starting hand types; 
     returns the results as a 2d list with final values as the outer list 
     and starting values as the inner list. *)
  let rec brute_force n current_hand_size storage = 
    if (n=0) then storage else 
      let (current_hand,drawn) = PokerEval.draw_n_cards current_hand_size [] in 
      let (next,final_hand) = PokerEval.draw_n_cards (7 - current_hand_size) 
        drawn in 
      let current_rank = PokerEval.get_rank_hand current_hand  in 
      let final_rank = PokerEval.get_rank_hand final_hand in
      brute_force (n-1) current_hand_size (plus1 final_rank current_rank 
        storage)

  (* Brute-forces probabilities of final hand types *)
  let rec force_final n lst = 
    if (n=0) then lst else 
      let (final_hand,drawn) = PokerEval.draw_n_cards 7 [] in 
      let final_rank = PokerEval.get_rank_hand final_hand in 
      force_final (n-1) (add1 final_rank lst)
       
  (* Brute-forces probabilities of opponent current hand type given board hand 
  type *)
  let rec force_opp_current n board_size storage = 
    if (n=0) then storage else 
      let (board,drawn) = PokerEval.draw_n_cards board_size [] in 
      let (pocket,opponent_hand) = PokerEval.draw_n_cards 2 drawn in 
      let board_rank = if (List.length board = 0) then 44 else 
        PokerEval.get_rank_hand board in 
      let opp_current_rank = PokerEval.get_rank_hand opponent_hand in 
      force_opp_current (n-1) board_size (plus1 opp_current_rank board_rank 
        storage)
      
  (* Brute-forces probabilities of winning and losing given board hand type and 
  current hand *) 
  let rec compare_finals n storage = 
    if (n=0) then storage else 
      let (board,board_drawn) = PokerEval.draw_n_cards 5 [] in 
      let (bpp_pocket,bpp_hand) = PokerEval.draw_n_cards 2 board_drawn in 
      let (opp_pocket,opp_hand) = PokerEval.draw_n_cards 2 board_drawn in 
      let bpp_final_rank = PokerEval.get_rank_hand bpp_hand in 
      let opp_final_rank = PokerEval.get_rank_hand opp_hand in 
      compare_finals (n-1) (plus1 bpp_final_rank opp_final_rank storage)

  (* Reveal brute force functions outside of module ; see CPT_I_Love_You.ml and
  probtransr.ml for their usage. CPT_I_Love_You has piecewise math functions *)
  let get_vals n cards_known = tabulate (brute_force n cards_known 
    start_tables)
  let get_final n = tabulate ([force_final n start_list]) 
  let get_board n board_size = tabulate (force_opp_current n board_size 
    start_tables)
  let get_compare_finals n = tabulate (compare_finals n start_tables)

end


open PokerEval
open Pokabayes 

(*
Deal 2 cards to both players

Player 1 looks at their cards, and decides whether to check or bet
|--
|If Player 1 checked, then Player 2 can check or bet
|If Player 1 bet, then Player to can call or raise
|--
|If Player 2 raises/bets, then move back with Player 2 as Player 1
|If Player 2 calls/checks, move to Round 2 
|--


3 cards get dealt to the board. 

Player 1 looks at their cards, and decides whether to check or bet
|--
|If Player 1 checked, then Player 2 can check or bet
|If Player 1 bet, then Player to can call or raise
|--
|If Player 2 raises/bets, then move back with Player 2 as Player 1
|If Player 2 calls/checks, move to Round 2 
|--


1 more card gets dealt to the board. 

Player 1 looks at their cards, and decides whether to check or bet
|--
|If Player 1 checked, then Player 2 can check or bet
|If Player 1 bet, then Player to can call or raise
|--
|If Player 2 raises/bets, then move back with Player 2 as Player 1
|If Player 2 calls/checks, move to Round 2 
|--


1 more card gets dealt to the board. 

Player 1 looks at their cards, and decides whether to check or bet
|--
|If Player 1 checked, then Player 2 can check or bet
|If Player 1 bet, then Player to can call or raise
|--
|If Player 2 raises/bets, then move back with Player 2 as Player 1
|If Player 2 calls/checks, move to Round 2 
|--

*)


(* lets me manage debugging printouts after the fact as I see fit *)
let print_debug s = 
  if Switches.debug_messages then prerr_endline ("Controller: "^s)
  else ()

(* takes two lists of strings. Reads a line of input. Returns true if 
  the input is in the first list, false if in the second, 
  and asks for a retry if the input is in neither. *)
let rec get_opt o1 o2 : bool= 
  let inp = String.lowercase (read_line()) in
  if (List.exists (fun o -> o = inp) o1) then true
  else if (List.exists (fun o -> o = inp) o2) then false
  else (print_string "Retry\n>> "; get_opt o1 o2)

(* Print welcome and ask (with get_opt) whether the human wants to play. *)
let greeting () : bool =
  print_string ("\n\nOh hai, Mark!\nThis is the LANE-DANA-SHANYI-ROBBIE"
  ^" POKERBOT 2000! \n\nAre you ready to play poker with a"
  ^" computer (y/n)? \n>> ");
  let cont = get_opt ["y"; "yes"] ["n";"no"] in
  print_endline "\n"; cont

(* Ask whether the human wants to go first. Return a bool of the answer. *)
let opp_goes_first () : bool = 
  print_string "Would you like to go first or second (1/2)? \n>> "; 
  let order = get_opt ["1"; "first"; "1st"] ["2"; "sec"; "second"; "2nd"] in
  print_endline "\n\n"; order

(* Print a message showing the human's starting hand for use
  before the game proper begins. *)
let print_player_starting_hand p1 p2 : unit= 
  let print_hand hnd = 
    print_string 
    ("***Your starting hand is: ***\n"
    ^(PokerEval.string_of_hand hnd)) in
  print_hand (if p1#id = Players.humanid then p1#hand else p2#hand)

(* ask if human wants to play again. *)
let get_newgame () = 
  print_debug "Ask if will play again. "; 
  print_string "Would you like to play again (y/n)?\n>> ";
  let play_again = get_opt ["y"; "yes"] ["n";"no"] in
  (if play_again then print_debug "Human says will play again. "
  else print_debug "Human says will NOT play again. ");
  print_endline "\n"; play_again

(* print a message saying that this is a new game. *)
let print_newgame () = 
  print_endline "\n\n*****NEW GAME*****\n\n"

(* Given the computer and the human players respectively, 
  print the final pot and hands of each player, and determine the winner
  and print who won. *)

let select_winner (bpp:Players.player_t) (opp:Players.player_t) = 
  let notify (message:string) =
    print_endline 
    ("\n\n*****"^message^" with a final pot of "
    ^(string_of_int 
      (if bpp#pot_amt > opp#pot_amt then bpp#pot_amt else opp#pot_amt))
      ^". *****\n"
    ^ "Your final hand was: \n"
      ^(PokerEval.string_of_hand (opp#hand@opp#board))^"\n"
    ^ "The computer's final hand was: \n"
      ^(PokerEval.string_of_hand (bpp#hand@bpp#board))^"\n\n") in
  (match 
    PokerEval.compare_hands ((bpp#hand)@(bpp#board)) ((opp#hand)@(opp#board))  
  with
  | 1 -> notify "The computer has won" 
  | (-1) ->  notify "You have won"
  | 0 -> notify "You and the computer have tied"
  | _ -> () )


(* Past this point there should be no IO. Everything here 
  is control flow. *)

(* rounds is the main loop of a game. It will be called 
  five times in a game that goes to finish. p1 is the player who is 
  going first on each turn in this game; p2 second. rounds returns 
  the round number from 1 to 5 of the last round of the game (it returns 5
  if the game went all the way through) *)
let rec rounds 
  (p1:Players.player_t) (p2:Players.player_t) (rn:int) 
  (all_cards_yet_dealt:PokerEval.deck) (prev_player: Players.player_t)
  : int = 
  (* round is a recursive function where each call is a betting turn.
    p1 is the player that will be asked to take an action this turn. 
    p2 is the other player. prev_player is strictly the player who made the 
    last action (this is usually p2, but can be p1 if this is the first 
    turn of a round. *)
  let rec turns 
    (p1:Players.player_t) (p2:Players.player_t) (prev_player:Players.player_t) 
    (first_turn_of_round : bool)
    : bool * Players.player_t = 
    let a = 
      (* someplayer#last is the last action that that player took, regardless
      of round boundaries. 
      a = the action that p1 wants to take*)
      if first_turn_of_round then p1#first_turn prev_player
      else p1#turn p2#last in 
    (match a with
    (* Return false (i.e. do not continue the game) if p1 folded. 
    Also return p1 but nothing will be done with it. *)
    | PokaBayes.Fold -> false, p1
    (* Else if p1 called, return true and p1 (who will later be passed in 
    to round in the next round as prev_player. The round is over. *)
    | PokaBayes.Call -> true, p1
    (* Else if p1 checked, and the preceding action was a check, and this is *not* 
    the first turn of a round, return as above*)
    | PokaBayes.Check -> 
      if prev_player#last = a && not(first_turn_of_round) then true, p1
    (* Else run round again with p2 as the new p1, p1 as the new p2, 
      and p1 as the new prev_player. It is not the beginning of a round, 
      so first_turn_of_round is false *)
      else turns p2 p1 p1 false
    | _ -> turns p2 p1 p1 false) in
  (* round_business handles the repeated business of running a round. *)
  let round_business (last_player:Players.player_t) (new_all:PokerEval.deck) =
    print_debug ("Round " ^ (string_of_int rn));
    (* set p1 and p2 to their start-of-round values / increment their 
      round counters. *)
    p1#new_round; p2#new_round; 
    (* get whether to continue and the last player of the prec. round *)
    let cont, last_player = turns p1 p2 last_player true in
    (* if continue, run rounds again to start at the next round. *)
    if cont then rounds p1 p2 (rn+1) new_all last_player
    (* else return the current round number, which will also then be returned 
      by rounds. *) 
    else rn in 
  (* round-specific procedures, viz: dealing, and printing the starting
    hand in the first round*)
  (match rn with 
  | 1 -> 
    let p1cs, new_all = PokerEval.draw_n_cards 2 all_cards_yet_dealt in
    let _ = p1#deal_to_player p1cs in
    let p2cs, new_all = PokerEval.draw_n_cards 2 all_cards_yet_dealt in
    let _ = p2#deal_to_player p2cs in
    print_player_starting_hand p1 p2; 
    round_business prev_player new_all
  | 2 ->
    let cs, new_all = (PokerEval.draw_n_cards 3 all_cards_yet_dealt) in
    let _, _ = (p1#deal_to_board cs), (p2#deal_to_board cs) in
    round_business prev_player new_all
  | 3 -> 
    let cs, new_all = (PokerEval.draw_n_cards 1 all_cards_yet_dealt) in
    let _, _ = (p1#deal_to_board cs), (p2#deal_to_board cs) in
    round_business prev_player new_all
  | 4 -> 
    let cs, new_all = (PokerEval.draw_n_cards 1 all_cards_yet_dealt) in
    let _, _ = (p1#deal_to_board cs), (p2#deal_to_board cs) in
    round_business prev_player new_all
  (* if rounds has been started with a round number greater than four then 
    the game must be over. Return the round number. *)
  | _ -> rn)
;;

(* the game loop. Each call of this function is a game. 
  bpp is the computer player. opp is the human player. opp_first iff human
  goes first. *)
let rec games (bpp:Players.player_t) (opp:Players.player_t) (opp_first:bool)
  : unit = 
  let rn = 
    print_debug "Starting game. "; 
    if opp_first then 
      rounds opp bpp 1 [] bpp 
    else rounds bpp opp 1 [] opp in 
  (match rn with
  | 5 -> 
    print_debug "Game ends in a comparison. "; 
    select_winner bpp opp 
  (* if the ending round number is less than 5 then someone must have folded. *)
  | _ -> 
    print_debug ("Game ends in a fold at round "^(string_of_int rn)); 
    ());
  if get_newgame() then 
    (print_newgame(); 
    (* player#reinit returns a completely re-initialized version of the player *)
    games (bpp#reinit) (opp#reinit) (if opp_first then false else true))
  else Players.print_goodbye ()

let _ = print_debug "Init System. ";;

let _ = print_debug "Initial Instantiate Players. " ;;
(* initially instantiate the players *)
let opp = new Players.oppl ;;
let bpp = new Players.bppl opp ;;

let _ = print_debug "Print Greeting and ask if to continue. " ;;
if greeting() then
  (print_debug "Human says will play. "; 
  games bpp opp (opp_goes_first()))
else ()

(* the game controller will handle the input/output with the player, 
   deal the cards, know the human's hand, and will call the functions 
   in Probabilistic to make the computer player go. *)

module Probabilistic = 
sig 
  (* a gamestate contains information about the state of the game
     and also contains the Bayesian network. It is, in a sense, the 
     state of the computer opponent. *)
  type gamestate
  type result = Win of int | Lose of int | Draw of int
  type update
  type bet = Fold | Check | Call | Raise of int
  type suit = Hearts | Diamonds | Clubs | Spades
  type card_val = int
  type card = (card_val * suit)
  type 'a group
  type pot = int
  type odds
  
(* This returns a gamestate as it will be at the very beginning of the game,
   before anything has been dealt. *)
  val new_game : gamestate
(* this function deals cards (packaged as a group) to the communal cards. *)
  val deal_to_board : gamestate -> card group -> gamestate
(* this function gives a group of cards to the computer player *)
  val starting_deal : gamestate -> card group -> gamestate
(* this function can be called when the human opponent folds. 
   it returns a result of the game, and an update, which will (if we get
   that far) allow the computer player to learn about each user. *)
  val opponent_folds : gamestate -> (result * update)
(* if the players reach the point where they must reveal their cards to each 
   other, the game controller tells the computer player what the human has*)
  val reveal_cards : gamestate -> card group -> (result * update)
  val computer_bet : gamestate -> bet option -> (bet*gamestate)
   
end
  
(* Unexposed function signatures *)
  val our_odds : gamestate -> odds
  val opponent_odds : gamestate -> odds
  val hand_value : gamestate -> int
  val calc_winner : int -> int -> result
  val increase_pot : pot -> pot
  val distribute_pot : pot -> pot

(* Human interface functions *)
  val opponent_bet : gamestate -> bet option -> (bet*gamestate)
  val display_gamestate : gamestate -> unit

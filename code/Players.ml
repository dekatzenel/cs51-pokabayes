open PokerEval
open Pokabayes 

let print_goodbye () =
    print_endline "Ahaha, what a story, Mark!"

let string_of_act act = 
  match act with
  | PokaBayes.NoAct -> "NO ACT"
  | PokaBayes.Check -> "CHECK"
  | PokaBayes.Call -> "CALL"
  | PokaBayes.Raise -> "RAISE"
  | PokaBayes.Bet -> "BET"
  | PokaBayes.Fold -> "FOLD"


module TestingBPP =
struct
  let turn a b c o_act e f = 
    match o_act with 
    | PokaBayes.NoAct -> PokaBayes.Check
    | PokaBayes.Check -> PokaBayes.Bet
    | PokaBayes.Raise -> PokaBayes.Raise
    | PokaBayes.Call -> PokaBayes.Call
    | PokaBayes.Bet -> PokaBayes.Raise
    | PokaBayes.Fold -> raise (Failure "FOLD should never reach turn -- TestingBPP")
end

let humanid = "Human" 
let compid = "Computer"


class type player_t = 
object 
(* Public methods.  *)
  method deal_to_player : PokerEval.deck -> unit
  method deal_to_board : PokerEval.deck -> unit
  method hand : PokerEval.deck
  method board : PokerEval.deck
  method round_amt : int
  method pot_amt : int
  method print_own_fold : unit
  method print_bpp_fold : unit
  method new_round : unit
  method turn : PokaBayes.action -> PokaBayes.action
  method first_turn : player_t -> PokaBayes.action
  method last : PokaBayes.action
  method id : string
  method reinit : player_t

(* The following methods needed to be technically public for inheritance 
  reasons but should **never** be called from outside. *)
  method more_new_round : unit
  method turn1r : player_t -> PokaBayes.action
  method more_reinit : unit
  method print_debug : string -> unit
  method do_action : PokaBayes.action -> PokaBayes.action
  method get_action : PokaBayes.action -> PokaBayes.action
end 


class player : player_t =
object (self)
  val mutable last_action = PokaBayes.NoAct

  val mutable on_board = []
  val mutable current_hand = []
  val mutable round = 0
  val mutable pot = 0

  method id = "PLAYER"

  method print_debug s = 
    if Switches.debug_messages then prerr_endline (self#id^" Player: "^s)
    else ()   

  method deal_to_player cards = current_hand <- cards@current_hand

  method deal_to_board cards = on_board <- cards@on_board

  method hand = current_hand

  method board = on_board

  method last = last_action

  method pot_amt = pot

  method round_amt = round

  method print_bpp_fold = ()

  method print_own_fold = ()

  method more_new_round = ()

  method new_round = round <- round + 1; self#more_new_round

  method do_action new_act = 
    self#print_debug ("My action is: "^(string_of_act new_act)); 
    last_action <- new_act;
    match new_act with
    | PokaBayes.Fold -> 
      self#print_own_fold; 
      last_action <- new_act;
      new_act
    | PokaBayes.Raise -> 
      pot <- pot + 2; 
      last_action <- new_act;
      new_act
    | PokaBayes.Bet -> 
      pot <- pot + 1; 
      last_action <- new_act;
      new_act
    | PokaBayes.Call -> 
      pot <- pot + 1; 
      last_action <- new_act;
      new_act
    | PokaBayes.Check | PokaBayes.NoAct -> 
      last_action <- new_act;
      new_act


  method get_action o_action = raise (Failure "cannot call this")

  method turn1r prev_player = raise (Failure "can't call this")

  method first_turn prev_player = 
    let p_action, p_id = prev_player#last, prev_player#id in 
    (match p_action with
    | PokaBayes.NoAct -> ()
    | PokaBayes.Check -> 
     (let subverb, poss = 
        match p_id with
        | "Computer" -> "The Computer has", "your"
        | "Human" -> "You have", "the Computer's"
        | _ -> raise (Failure "unrecognized id") in
      print_endline 
       ("\n\n"
        ^subverb
        ^" responded to "
        ^poss
        ^" CHECK with a CHECK, so the " 
        ^(if self#round_amt < 5 then "round" else "game")
        ^" is now over. The pot is currently "
        ^(string_of_int self#pot_amt)^". "))
    | PokaBayes.Call -> 
     (let subverb, poss = 
        match p_id with
        | "Computer" -> "The Computer has", "your"
        | "Human" -> "You have", "the Computer's"
        | _ -> raise (Failure "unrecognized id") in
      print_endline 
       ("\n\n"
        ^subverb
        ^" CALLed, so the " 
        ^(if self#round_amt < 5 then "round" else "game")
        ^" is now over. The pot is currently "
        ^(string_of_int self#pot_amt)^". "))
    | _ -> raise (Failure "Cannot call first_turn after a raise/bet/fold"));
    (let fragment = 
      (if self#id = humanid then "You go" else "Computer goes") in
    print_endline 
      ("\n\n\n*****This is round "^(string_of_int self#round_amt)^". "
      ^fragment^" first.*****")); 
    self#turn1r prev_player



  method turn o_action = 
    self#print_debug "Init Turn. ";
    self#print_debug ("I received action "^(string_of_act o_action)); 
    match o_action with
    | PokaBayes.Fold -> raise (Failure "non-folder should never receive Fold")
    | PokaBayes.Raise -> 
      pot <- pot + 2; 
      self#get_action o_action
    | PokaBayes.Bet ->
      pot <- pot + 1; 
      self#get_action o_action
    | PokaBayes.Call -> 
      pot <- pot+1 ; 
      self#get_action o_action
    | PokaBayes.Check | PokaBayes.NoAct -> 
      self#get_action o_action

  method more_reinit = ()

  method reinit : player_t = 
    last_action <- PokaBayes.NoAct; 
    on_board <- [];
    current_hand <- [];
    round <- 0;
    pot <- 0;
    self#print_debug "Reinitializing self. "; 
    self#more_reinit; 
    (self :> player_t)

end





class oppl : player_t = 
object (self)
  inherit player as super

  method id = "Human"

  method private avail_action m = 
    match m with
    | PokaBayes.NoAct | PokaBayes.Check | PokaBayes.Call -> 
      ((PokaBayes.Bet, "BET"),(PokaBayes.Check, "CHECK"))
    | PokaBayes.Bet | PokaBayes.Raise -> 
      ((PokaBayes.Raise, "RAISE"),(PokaBayes.Call, "CALL"))
    | _ -> raise (Failure "opp avail_action_str unexpected action")

  method private avail_act_pr_str o_action = 
    let (_,a), (_,b) =  self#avail_action o_action in
    a^", "^b^", or FOLD"


  method private print_game_prompt o_action = 
    (if not(o_action = PokaBayes.NoAct) then 
      print_string 
        ("\n\nThe computer's action was to "^(string_of_act o_action)
        ^" which resulted in a pot of "^(string_of_int self#pot_amt)^".\n") 
    else ());
    print_string 
    ("You have in hand: \n"^(PokerEval.string_of_hand self#hand)
    ^(if not(self#board = []) then
        ("\nOn the board are the following cards:\n"
        ^(PokerEval.string_of_hand self#board))
      else "\nThere are no cards on the board.\n")
    ^"\nWould you like to "^(self#avail_act_pr_str o_action)^"? \n>> ")


  method get_action o_action = 
    let rec grab o_action = 
      let (act,a),(bct, b) = (self#avail_action o_action) in
      let inp =  String.uppercase(read_line()) in
      if inp = a then act
      else if inp = b then bct
      else if inp = "FOLD" then PokaBayes.Fold
      else (print_string "Retry: "; grab o_action) in
    self#print_debug "Printing game prompt. "; 
    self#print_game_prompt o_action; 
    self#do_action (grab o_action)

  method turn1r prev_player = self#turn PokaBayes.NoAct
 
  method print_bpp_fold = 
    print_endline 
      ("\n\nThe computer has folded. You win "
        ^(string_of_int self#pot_amt)^".") 
  method print_own_fold = 
    print_endline 
      ("\n\nYou have folded. The computer wins "
        ^(string_of_int self#pot_amt)^".") 
end






class bppl op : player_t =
object (self)
  inherit player as super
  
  val mutable internal_last_action = PokaBayes.NoAct


  method more_new_round = internal_last_action <- PokaBayes.NoAct

  method private int_last = internal_last_action

  method id = "Computer"

  method private print_own_fold = op#print_bpp_fold

  method private print_bpp_fold = self#print_own_fold

(*
get_action is currently calling a dummy "turn" function, 
TestingBPP.turn, instead of the actual AI. This is so that I could
test the controller. Replace TestingBPP.turn with the name 
of the actual thing that needs to be called to use the network. 
*)
  method get_action o_act = 
    self#print_debug "Inputting state into Pokabayes and awaiting reply. "; 
    let a = 
      (PokaBayes.turn 
        ((PokerEval.get_rank_hand (self#hand@self#board))-1)
        self#round_amt
        self#int_last
        o_act
        ((PokerEval.get_rank_hand self#board)-1)
        self#pot_amt) in
    internal_last_action <- a; 
    self#do_action a

  method turn1r prev_player = self#turn op#last

  method more_reinit = 
    internal_last_action <- PokaBayes.NoAct

end

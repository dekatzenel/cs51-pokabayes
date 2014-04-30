(* Uncomment to make everything abstract, if you must. 
   Also uncomment the module struct's ": POKEREVAL"
module type POKEREVAL =
sig 
  (* see struct comments for details *)
  type card = char*int
  
  (* decks are a list of all the already-drawn cards in a deck *)
  type deck = card list
  
  (* set at true to group 44 abstractions instead of 9 *)
  val full_abstraction : bool 

  (* the rank corresponding with the hand abstraction 
     either 1-9 or 1-44 depending on full_abstraction bool *)
  val get_rank_hand : card list -> int
 
  (* draws n cards and gives the deck resultant from the draw *)
  val draw_n_cards : int -> deck -> card list*deck

  (* string of a hand *)
  val string_of_hand : card list -> string
end
  *)

(*********************************************************************

   Table of Contents for module struct PokerEval

   Bools
       rank_suits  -- do not change this to true
       full_abstraction 
   Type Declarations
       card, deck, hand, expandedhand, strhand, broadhand
   Basic Deck and Card Functions
       draw_card deck -> card*deck
       draw_n_cards n deck -> card list*deck
   General Helper Functions
       c_suit_order char char -> char
       strip_to_chars card list -> char list
       strip_to_ints card list -> int list
       high_val card list -> int
       rev_comp int int -> int
       card_compare(_no_suit) card card -> int
       sort card list -> card list
       suit_counts_list (card list) bool -> int list
       highest_suit_count card list -> int
       suit_of_highest_count card list -> char
       value_of_n_reps (card list) int -> int list
       samesuits char list -> bool
       rem_dupl_vals int list -> int list
         pack 'a list -> 'a list  -- from the internet
       count_consec card list -> int
       nums_same int list -> int*int
       straight card list -> bool 
       filter card list -> bool
   9-Abstraction Helper Functions
       handtype_(2/3/4/5) card list -> hand
   9-Abstraction Functions
       handtype card list -> hand
       sub_rank card list -> int
   expandedhand Abstraction Helper Functions
       sub_(busted/pair/two_pair/three/four) card list -> expandedhand
   Full Abstraction Functions
       expandedhandtype card list -> expandedhand
       strhandtype card list -> strhand
       broadhandtype card list -> broadhand
   Full Ranking Functions 
       rank expandedhand str flu -> int
       get_beta_rank_hand card list -> int
       get_rank_hand card list -> int
   Hand Comparison Functions
       val_list_comp (card list) (card list) int -> int
       multi_comp (card list) (card list) int -> int
       order_of_str card list -> card
       order_of_fl_str card list -> card
       order_of_fl card list -> card
       full_house_comp (card list) (card list) -> int
       two_pair_comp (card list) (card list) -> int
       compare_hands (card list) (card list) -> int
   Card Representation Functions
       string_of_card card -> string
       string_of_hand card list -> string
         implode  -- from the internet
       prim_string card list -> string
   Test Functions

**********************************************************************)


module PokerEval (*: POKERVAL *) =
struct 

(*******************)
(*****  BOOLS  *****)
(*******************)


(* change this value to true to rank suits
   suits are not generally ranked in Texas Hold'Em 
   tie hands are still possible with suit ranks
*)

let rank_suits = false

(* change this value to true to use FULLABSTRACTION ranking 
   instead of the 9 abstractions used in POKEREVAL.
*)

let full_abstraction = true


(*******************************)
(*****  TYPE DECLARATIONS  *****)
(*******************************)


(* char maps to suit: 
     c -> club
     d -> diamond
     h -> heart
     s -> spades
   int maps to card value
     2-10 -> 2-10
     11 -> Jack
     12 -> Queen
     13 -> King
     14 -> Ace
*)

type card = char*int

(* a deck is a card list of the already-drawn cards from a deck *)

type deck = card list

(* hand abstractions to simplify probabilities in Bayesian tree 
   card list allows for ranking internal to the abstractions
*)

type hand =  Busted of card list
            | Pair of card list
            | Two_Pair of card list
            | Three of card list
            | Straight of card list
            | Flush of card list
            | Full_House of card list
            | Four of card list 
            | Straight_Flush of card list

(* Busted Low -> high card <9
   Busted High -> >8 <Q
   Busted Royal -> Q/K
   Busted Ace -> Ace

   Pair Low -> Pair lower than 9
   Pair High -> Pair lower than queen
   Pair Royal -> Queen/King
   Pair Ace -> Ace
   
   Low Three -> Lower than Jack
   High Thee -> Higher than 10

   Low Four -> Lower than Jack
   High Four -> Higher than 10

   Low Two Pair -> Highest is lower than Jack
   High Two Pair -> Higher than 10 

   Other -> Category for other hands
*)
type expandedhand = | Norm of hand
	          | Busted_Low of card list
              | Busted_High of card list
              | Busted_Royal of card list
              | Busted_Ace of card list
              | Pair_Low of card list
              | Pair_High of card list
              | Pair_Royal of card list
              | Pair_Ace of card list
              | Two_Pair_Low of card list
              | Two_Pair_High of card list
              | Three_Low of card list 
              | Three_High of card list
              | Four_Low of card list
              | Four_High of card list

            
(* These are "umbrella" types that will be used to judge future success
   in Conditional Probability Tables but not to judge final hands.
*)
type strhand = Not of expandedhand | Possible_Straight of expandedhand

type broadhand = Other of strhand | Possible_Flush of strhand


(*******************************************)
(*****  BASIC DECK AND CARD FUNCTIONS  *****)
(*******************************************)
let rand : int -> int = Random.self_init () ; Random.int

(* draws a card from a deck *)
let rec draw_card (d: deck) : card*deck =
  let generate_card = match rand 4, (rand 13)+2 with
    | 0, x -> ('s',x)
    | 1, x -> ('h',x)
    | 2, x -> ('d',x) 
    | 3, x -> ('c',x) 
    | _ -> ('z',0) in
  let c = generate_card in
  if List.exists (fun a -> a=c) d then draw_card d else c,(c::d);;
  
(* draws n cards from a deck *)
let draw_n_cards (n: int) (d: deck) : card list*deck = 
  let new_n = min (52-(List.length d)) n in
  let rec cardget m clst dlst = 
    if m = 0 then clst,dlst
    else match draw_card dlst with
    | (g,h) -> cardget (m-1) (g::clst) h in 
  cardget new_n [] d
  

(**************************************)
(*****  GENERAL HELPER FUNCTIONS  *****)
(**************************************)


(* orders suits for cards. used even without suit ordering to make 
   hands with equivalent cards unique once sorted
*)

let c_suit_order (a:char) (b: char) : char =
  match a,b with 
  | 's', _ -> a
  | _, 's' -> b
  | 'h', _ -> a
  | _ , 'h' -> b
  | 'd', _ -> a
  | _ , 'd' -> b
  | 'c', _ -> a
  | _ -> a

(* strips to chars, strips to ints*)
let strip_to_chars (lst:card list) : char list =
  let rec helper lst lstnew = 
    match lst with 
    | (c,v)::tl -> helper tl (lstnew@[c]) 
    | [] -> lstnew in
  helper lst []

let strip_to_ints (lst:card list) : int list =
  let rec helper lst lstnew = 
    match lst with    
    | (c,v)::tl -> helper tl (lstnew@[v])
    | [] -> lstnew in 
  helper lst []

(* stripps ints of a sorted card list and finds highest value *)
let high_val (h:card list) : int =
  match List.rev (strip_to_ints h) with
  | hd::tl -> hd 
  | [] -> 0

(* used to sort lists from high to low for less use of List.rev *)
let rev_comp (a: int) (b: int) : int = 
  match a > b, a = b with 
  | true, _ -> -1
  | _, true -> 0
  | _ -> 1

(* return a positive int if the first card is greater according to 
   value or, if neccessary, suit order. returns a negative int if the 
   second is greater. 
*)

let card_compare (a:card) (b:card) : int = 
  let ((s1,v1),(s2,v2)) = (a,b) in
  match (v1=v2), (v1=max v1 v2), (s1=c_suit_order s1 s2) with
  | true, _, true -> 1
  | true, _, _ -> -1
  | _, true, _ -> 1
  | _, _, _ -> -1

let card_compare_no_suit (a: card) (b: card) : int =
  let ((s1,v1),(s2,v2)) = (a,b) in
  match (v1=v2), (v1=max v1 v2) with
  | true,_ -> 0
  | _, true -> 1
  | _ -> -1

(* sorts a list of cards so that they go in numerical and suit order 
   each hand is unique when passed through this sorting function, i.e.
   equality holds between equivalent hands once they are passed through
*)

let rec sort (lst: card list) : card list = 
  List.sort card_compare lst

(* bule = true returns the list of suit counts in order from highest 
   to lowest suit count. 
   bule = false returns the list of suit counts in the order 
   clubs, diamonds, hearts, spades. 
*)
let suit_counts_list (h: card list) (bule:bool) : int list = 
  let rec fill_list (h1: card list) (c: int) (d: int) (h:int) (s:int) 
  : int list = 
    match h1 with 
    | (cha,_)::tl -> (
      match cha with 
      | 'c' -> fill_list tl (c+1) d h s
      | 'd' -> fill_list tl c (d+1) h s
      | 'h' -> fill_list tl c d (h+1) s
      | 's' -> fill_list tl c d h (s+1)
      | _ -> fill_list tl c d h s )
    | [] -> if bule then List.sort (rev_comp) [c;d;h;s] 
      else [c;d;h;s] in
  fill_list h 0 0 0 0 

let highest_suit_count (h: card list) : int = 
  match suit_counts_list h true with
  | hd::_ -> hd
  | [] -> 0 

let suit_of_highest_count (h: card list) : char = 
  let high = highest_suit_count h in 
  let rec helper (l:int list) (i:int) =
    match l with
    | hd::tl -> if hd = high then i else helper tl i+1 
    | [] -> 5 in
  match helper (suit_counts_list h false) 0 with 
  | 0 -> 'c' | 1 -> 'd' | 2 -> 'h' | 3 -> 's' | _ -> 'z'

(* Always pass in a SORTED CARD LIST for expected behavior. 
   Always pass in n > 1 for expected behavior. 
   Returns a list of card values that repeat n times in h
   with the highest value first stopping after two list elements
*)
let value_of_n_reps (h: card list) (n:int) : int list =
  let rec helper l i =
    match l with 
    | hd::tl -> 
      let nl = List.filter (fun a->not(a=hd)) tl in 
      if (List.length tl) - (List.length nl) = n-1 then (
        if i = 0 then helper nl hd else [i;hd])
      else helper nl i 
    | [] -> [i] in
  helper (List.rev (strip_to_ints h)) 0 

(* goes through a char list of indeterminate size and determines if 
   at least five cards are the same char
*)
    
let samesuits (lst:char list) : bool = 
  let rec helper (lst: char list) (lesslst:char list): bool =
    match lesslst with 
    | hd::tl -> 
      (not(List.length (List.filter (fun a -> a=hd) lst) < 5) 
      || helper lst tl)
    | [] -> false in
  helper lst lst

(* removes all consecutive identical values 
   useful for simplifying straight-finding algorithm 
*)
let rem_dupl_vals (lst: int list) : int list=
  let lst = List.sort compare lst in
  let rec helper lst = 
    match lst with
    | [] -> [] | [_] -> lst
    | h1 :: ((h2 :: _) as tl) ->
        if h1 = h2 then helper tl else h1 :: helper tl
  in helper lst

(* ocaml.org/tutorials/99problems.html
  "Pack[s] consecutive duplicates of list elements into sublists."
  I'm trying to be lazy. Adapted to pack straights instead of 
  duplicates.
*)  
let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a+1 = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] (rem_dupl_vals list))

let count_consec (h: card list) : int = 
  match (List.sort rev_comp (List.map (List.length) 
    (pack (strip_to_ints h)))) with
  | hd::tl -> hd
  | [] -> 0

(* determines the number of same-value ints in a list 
   always call with num & num2 = 0 
*)

let nums_same (lst:int list): int*int =
  let rec helper lst num num2 = 
    match lst with 
    | hd::tl -> 
      let sames = List.length (List.filter (fun a ->hd=a) lst) in
      let notsames = List.filter (fun a ->not(hd=a)) lst in
      if max sames num = sames then helper notsames sames (max num num2)
      else if max sames num2 = sames then helper notsames num sames
      else helper notsames num num2
    | [] -> (num,num2) in 
  helper lst 1 1

(* true if an int list has a five consecutive ints *)
let straight (h: card list) : bool = 
  count_consec h > 4
 
(* returns true if a card has both a straight and a flush 
   in the same set, false if not. *** computationally expensive --
   use only if something certainly has a flush and a straight

   passing a full deck (empty list) takes on the order of days.
   passing 10 cards takes much less than a second. 
  
   passed a SORTED list
*)
let rec filter (h: card list) : bool = 
  if List.length h = 5 then (straight h) && 
    samesuits (strip_to_chars h) else
  match h with 
  | hd::hd2::tl -> 
    (match List.length (hd::tl) > 4, List.length (hd::tl) = 5,
      straight (hd::tl)
        && samesuits (strip_to_chars (hd::tl)),
      straight (hd2::tl)
        && samesuits (strip_to_chars (hd2::tl)) with
    | false,_,_,_ -> false
    | _,true,true,_ -> true
    | _,true,_,true -> true
    | _,true,_,_ -> false
    | _,_,true,true -> (filter (hd::tl))||(filter (hd2::tl))
    | _,_,true,_ -> filter (hd2::tl)
    | _,_,_,true -> filter (hd::tl)
    | _ -> false)
  | _ -> false
 

(********************************************)
(*****  9-ABSTRACTION HELPER FUNCTIONS  *****)
(********************************************)


(* assigns a pair of cards to either Pair or Busted abstractions *)
let handtype_2 (h: card list) : hand =
  let n,x = nums_same (strip_to_ints h) in
  if n = 2 then Pair(h) else Busted(h) 

(* assigns three cards to either Three, Pair, or Busted *)
let handtype_3 (h: card list) : hand =
  let n,x = nums_same (strip_to_ints h) in
  if n = 3 then Three(h) else if n=2 then Pair(h) else Busted(h)

(* assigns four cards to either Four, Three, Two_Pair, or Busted *)
let handtype_4 (h: card list) : hand =
  let n,x = nums_same (strip_to_ints h) in 
  if n = 4 then Four(h) else if n=3 then Three(h) 
    else if n=2 && x=2 then Two_Pair(h) else if n=2 then Pair(h) 
    else Busted(h)

(* assigns five or more cards to an abstraction *)
let handtype_5 (lst: card list) : hand = 
  let suitlst = strip_to_chars lst in
  let numlst = strip_to_ints lst in 
  let fl = samesuits suitlst in 
  let st = straight lst in
  let nu,nu1 = nums_same numlst in
  match nu,nu1,st,fl with
  | 4,_,true,true -> 
    if filter lst then Straight_Flush(lst) else Four(lst)
  | 3,2,true,true -> 
    if filter lst then Straight_Flush(lst) else Full_House(lst)
  | _,_,true,true -> 
    if filter lst then Straight_Flush(lst) else Flush(lst)
  | 3,1,false,false -> Three(lst)
  | 3,_,_,_ -> Full_House(lst)
  | _,_,false,true -> Flush(lst)
  | _,_,true,_ -> Straight(lst)
  | 2,2,_,_ -> Two_Pair(lst)
  | 4,_,_,_ -> Four(lst)
  | 2,_,_,_ -> Pair(lst)
  | _ -> Busted(lst) 


(*************************************)
(*****  9-ABSTRACTION FUNCTIONS  *****)
(*************************************)


(* places a card list into a hand abstraction *)
let handtype (h: card list) : hand =
  let lst = sort h in 
  let l = List.length lst in
  if l = 2 then handtype_2 lst 
  else if l = 3 then handtype_3 lst 
  else if l = 4 then handtype_4 lst 
  else handtype_5 lst

let sub_rank (h: card list) : int = 
  match handtype h with 
  | Straight_Flush(_) -> 1
  | Four(_) -> 2
  | Full_House(_) -> 3
  | Flush(_) -> 4
  | Straight(_) -> 5 
  | Three(_) -> 6
  | Two_Pair(_) -> 7
  | Pair(_) -> 8
  | Busted(_) -> 9


(*******************************************************)
(*****  EXPANDEDHAND ABSTRACTION HELPER FUNCTIONS  *****)
(*******************************************************)


let sub_busted (h:card list) : expandedhand =
  let hd = high_val h in
  match hd<9,hd<12,hd<14 with 
  | true,_,_ -> Busted_Low(h)
  | _,true,_ -> Busted_High(h)
  | _,_,true -> Busted_Royal(h)
  | _ -> Busted_Ace(h)

let sub_pair (h:card list) : expandedhand = 
  let v = List.hd (value_of_n_reps h 2) in
  match v<9, v<12, v<14 with 
  | true,_,_ -> Pair_Low(h)
  | _,true,_ -> Pair_High(h)
  | _,_,true -> Pair_Royal(h)
  | _ -> Pair_Ace(h)

let sub_two_pair (h:card list) : expandedhand = 
  let v = List.hd (value_of_n_reps h 2) in 
  match v<11 with
  | true -> Two_Pair_Low(h)
  | _ -> Two_Pair_High(h)

let sub_three (h:card list) : expandedhand = 
  let v = List.hd (value_of_n_reps h 3) in 
  match v<11 with
  | true -> Three_Low(h) 
  | _ -> Three_High(h)

let sub_four (h:card list) : expandedhand = 
  let v = List.hd (value_of_n_reps h 4) in 
  match v<11 with 
  | true -> Four_Low(h)
  | _ -> Four_High(h) 


(***************************************)
(*****  FULL ABSTRACTION FUNCTIONS *****)
(***************************************)


let expandedhandtype (h:card list) : expandedhand =
  match handtype h with 
  | Busted(x) -> sub_busted x 
  | Pair(x) -> sub_pair x
  | Two_Pair(x) -> sub_two_pair x
  | Three(x) -> sub_three x
  | Four(x) -> sub_four x
  | x -> Norm(x)

let strhandtype (h: card list) : strhand = 
  let len = List.length h in
  if expandedhandtype h = Norm(handtype h) ||  len > 6 
    then Not(expandedhandtype h)
  else let h = sort h in
    match (len>=(count_consec h) && len < ((count_consec h) *2))
      && not(count_consec h = 5) with 
    | true -> Possible_Straight(expandedhandtype h)
    | _ -> Not(expandedhandtype h)

let broadhandtype (h: card list) : broadhand = 
  let len = List.length h in
  if expandedhandtype h = Norm(handtype h) ||  len > 6 
    then Other(strhandtype h)
  else let h = sort h in 
    match (len >= (highest_suit_count h) && len < ((highest_suit_count h) *2))
      && not(highest_suit_count h = 5) with
    | true -> Possible_Flush(strhandtype h)
    | false -> Other(strhandtype h)


(***********************************)
(*****  FULL RANKING FUNCTIONS *****)
(***********************************)


(* Hands of the same general type are ranked within type by 
   the presence of possible flushes, and then within that by the 
   presence of possible straights. 
*)

let rank (h: expandedhand) (str: bool) (flu: bool) : int = 
  match h, str, flu with 
  | Norm(x),_,_ -> (
    match x with 
    | Straight_Flush(_) -> 1
    | Full_House(_) -> 4
    | Flush(_) -> 5
    | _ -> 6 )
  | Four_High(_),_,_ -> 2
  | Four_Low(_),_,_ -> 3
  | Three_High(_),_,_ -> 7
  | Three_Low(_),_,_ -> 8
  | Two_Pair_High(_),_,true -> 9
  | Two_Pair_High(_),_,_ -> 10
  | Two_Pair_Low(_),_,true -> 11
  | Two_Pair_Low(_),_,_ -> 12
  | Pair_Ace(_),true,true -> 13
  | Pair_Ace(_),_,true -> 14
  | Pair_Ace(_),true,_ -> 15
  | Pair_Ace(_),_,_ -> 16
  | Pair_Royal(_),true,true -> 17
  | Pair_Royal(_),_,true -> 18
  | Pair_Royal(_),true,_ -> 19
  | Pair_Royal(_),_,_ -> 20
  | Pair_High(_),true,true -> 21
  | Pair_High(_),_,true -> 22
  | Pair_High(_),true,_ -> 23
  | Pair_High(_),_,_ -> 24
  | Pair_Low(_),true,true -> 25
  | Pair_Low(_),_,true -> 26
  | Pair_Low(_),true,_ -> 27
  | Pair_Low(_),_,_ -> 28
  | Busted_Ace(_),true,true -> 29
  | Busted_Ace(_),_,true -> 30
  | Busted_Ace(_),true,_ -> 31
  | Busted_Ace(_),_,_ -> 32
  | Busted_Royal(_),true,true -> 33
  | Busted_Royal(_),_,true -> 34
  | Busted_Royal(_),true,_ -> 35
  | Busted_Royal(_),_,_ -> 36
  | Busted_High(_),true,true -> 37
  | Busted_High(_),_,true -> 38
  | Busted_High(_),true,_ -> 39
  | Busted_High(_),_,_ -> 40
  | _,true,true -> 41
  | _,_,true -> 42
  | _,true,_ -> 43
  | _ -> 44 

let get_beta_rank_hand (h:card list) : int =
  match broadhandtype h with 
  | Other(x) -> (
    match x with 
    | Not(x) -> rank x false false
    | Possible_Straight(x) -> rank x true false )
  | Possible_Flush(x) -> (
    match x with 
    | Not(x) -> rank x false true
    | Possible_Straight(x) -> rank x true true )

(* gets an integer rank with lower > higher of a hand *)
let get_rank_hand (h: card list) : int =
  if full_abstraction then get_beta_rank_hand h
  else sub_rank h 


(**************************************)
(*****  HAND COMPARISON FUNCTIONS *****)
(**************************************)


(* returns 0 if tied or fewer than 7 cards 
   returns 1 if the first hand is better
   returns -1 if the second hand is better
*)

let rec val_list_comp l1 l2 n : int =
  match n, List.rev (sort l1), List.rev (sort l2) with 
  | 5,_,_ -> 0
  | _,hd1::tl1, hd2::tl2 -> 
    if rank_suits then 
      (let x = card_compare hd1 hd2 in 
      if x = 0 then val_list_comp tl1 tl2 (n+1) else x)
    else  (let x = card_compare_no_suit hd1 hd2 in 
      if x = 0 then val_list_comp tl1 tl2 (n+1) else x)
  | _ -> 0

let multi_comp l1 l2 n : int = 
  match value_of_n_reps l1 n, value_of_n_reps l2 n with
  | hd1::_,hd2::_ -> 
    (let x = compare hd1 hd2 in 
    if x = 0 then val_list_comp 
      (List.filter (fun (a,b) -> not(b=hd1)) l1)
      (List.filter (fun (a,b) -> not(b=hd1)) l2) n
    else if x < 0 then -1 else 1)
  | _ -> 0

let rec order_of_str l : card = 
  match l with 
  | hd::tl -> 
    if not(straight tl) then List.nth tl 3  else order_of_str tl
  | [] -> ('z',0)

let rec order_of_fl_str l : card =
  match l with 
  | hd::tl -> 
    if not(filter tl) then List.nth tl 3  else order_of_str tl
  | [] -> ('z',0)

let order_of_fl l : card = 
  let c = suit_of_highest_count l in
  List.hd (List.rev (List.filter (fun (a,b) -> not(a=c)) l))

let full_house_comp l1 l2 : int = 
  match value_of_n_reps l1 3, value_of_n_reps l2 3 with
  | hd::tl,hd1::tl2 -> 
    (let x = compare hd hd1 in if x = 0 then (
      match value_of_n_reps l1 2, value_of_n_reps l2 2 with
      | hd::tl,hd1::tl2 -> compare hd hd1 
      | _ -> x )
    else x) 
  | _ -> 0

let two_pair_comp l1 l2 : int = 
  match value_of_n_reps l1 2, value_of_n_reps l2 2 with
  | hd::tl::[],hd1::tl1::[] -> 
    (let x,y = compare hd hd1,compare tl tl1 in  
    if x = 0 && y = 0 then (
      val_list_comp (List.filter (fun (a,b) -> not(b=hd || b=tl)) l1)
        (List.filter (fun (a,b) -> not(b=hd || b=tl)) l2) 4)
    else if x = 0 then y else x)
  | _ -> 0

let compare_hands (h1: card list) (h2: card list) : int =
  if not(List.length h1 = 7) || not(List.length h2 = 7) then 0 else (
    let h1,h2 = sort h1, sort h2 in
    let x = get_beta_rank_hand h1 in
    let y = get_beta_rank_hand h2 in
    if x<y then 1 else if x>y then -1 
    else match handtype h1 with
      | Busted(_) -> val_list_comp h1 h2 0
      | Pair(_) -> multi_comp h1 h2 2
      | Three(_) -> multi_comp h1 h2 3
      | Four(_) -> multi_comp h1 h2 4
      | Full_House(_) -> (if full_house_comp h1 h2 < 0 then -1 
        else if full_house_comp h1 h2 > 0 then 1 else 0)
      | Two_Pair(_) -> two_pair_comp h1 h2
      | Straight(_) -> (if rank_suits 
        then card_compare (order_of_str h1) (order_of_str h2)
        else card_compare_no_suit (order_of_str h1) (order_of_str h2))
      | Straight_Flush(_) -> (if rank_suits 
        then card_compare (order_of_fl_str h1) (order_of_fl_str h2)
        else card_compare_no_suit (order_of_fl_str h1) 
          (order_of_fl_str h2))
      | Flush(_) -> (if rank_suits 
        then card_compare (order_of_fl h1) (order_of_fl h2)
        else card_compare_no_suit (order_of_fl h1) 
          (order_of_fl h2))
  )


(*******************************************)
(*****  CARD REPRESENTATION FUNCTIONS  *****)
(*******************************************)


let string_of_card (input: card) : string =
  let suitstring c = 
    match c with 
    | 'c' -> "of Clubs"
    | 'd' -> "of Diamonds"
    | 's' -> "of Spades"
    | 'h' -> "of Hearts"
    | _ -> "of INVALID SUIT" in
  let numstring v = 
    match v with 
    | 2 -> "Two "
    | 3 -> "Three "
    | 4 -> "Four "
    | 5 -> "Five "
    | 6 -> "Six "
    | 7 -> "Seven "
    | 8 -> "Eight "
    | 9 -> "Nine "
    | 10 -> "Ten "
    | 11 -> "Jack " 
    | 12 -> "Queen "
    | 13 -> "King "
    | 14 -> "Ace "
    | _ -> "INVALID VALUE " in
  let (c,v) = input in ((numstring v)^(suitstring c))

let string_of_hand (h: card list) : string =
  let rec helper l s = 
    match l with 
    | hd::tl -> helper tl (s^(string_of_card hd)^("\n"))
    | [] -> s in
  helper (sort h) ""


(* below is for testing *)

(* from "Frequently asked Questions about Caml" *)
let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

let prim_string (h: card list) : string =
  let rec helper l s = 
    match l with 
    | hd::(hd1::_ as tl) -> let (c,v) = hd in 
      helper tl (s^"('"^(implode [c])^"',"^(string_of_int v)^");")
    | hd::tl -> let (c,v) = hd in 
      helper tl (s^"('"^(implode [c])^"',"^(string_of_int v)^")")
    | [] -> s^"]" in 
  helper (sort h) "["


(****************************)
(*****  TEST FUNCTIONS  *****)
(****************************)


let test_suit_order () = 
  assert(c_suit_order 'd' 's' = 's');
  assert(c_suit_order 'h' 'h' = 'h');
  assert(c_suit_order 'd' 'c' = 'd');
  assert(c_suit_order 'c' 'c' = 'c'); 
()

let card_compare_test () = 
  assert(card_compare ('s',5) ('d',5) = 1);
  assert(card_compare ('c',5) ('s',14) = -1);
  assert(card_compare ('c',8) ('s',6) = 1);
  assert(card_compare ('h',10) ('s',7) = 1);
  assert(card_compare ('s',9) ('s',10) = -1); 
()

let card_compare_no_suit_test () = 
  assert(card_compare_no_suit ('s',5) ('d',5) = 0);
() 

 let sort_test () = 
  let lst1 = [('c',5);('s',14);('h',10);('s',7);('s',9);('s',10);('s',3)] in 
  let lst2 = [('s',14);('h',10);('s',7);('c',5);('s',10);('s',9);('s',3)] in
  let lst3 = [('c',5);('s',5);('d',5);('h',5)] in
  let lst4 = [('s',5);('h',5);('c',5);('d',5)] in 
  let lst12 = [('s',3);('c',5);('s',7);('s',9);('h',10);('s',10);('s',14)] in
  let lst34 = [('c',5);('d',5);('h',5);('s',5)] in
  assert(sort lst1 = sort lst2);
  assert(sort lst12 = sort lst1);
  assert(lst12 = sort lst12);
  assert(sort lst3 = sort lst4);
  assert(sort lst4 = sort lst34);
  assert(lst34 = sort lst3);
  assert(not(lst1 = lst2));
  assert(not(lst3 = lst4));
  assert(not(lst34 = lst12)); 
() 

let nums_same_test () = 
  let lst1 = [1;2;3;4;5;6;7] in
  let lst2 = [2;3;4;2;6;7;8] in
  let lst3 = [2;3;4;5;2;3;8] in
  let lst4 = [4;1;4;3;2;1;1] in
  let lst5 = [4;4;2;2;2;1;2] in
  let lst6 = strip_to_ints (sort [('s',5);('h',3);('c',5);('d',3);('s',3)]) in
  assert(nums_same lst1 = (1,1));
  assert(nums_same lst2 = (2,1));
  assert(nums_same lst3 = (2,2));
  assert(nums_same lst4 = (3,2));
  assert(nums_same lst5 = (4,2));
  assert(nums_same lst6 = (3,2));
()

(* tests for every hand size *)
let handtype_2_test () = 
  let lst1 = [('c',5);('d',5)] in
  let lst2 = [('d',2);('h',3)] in
  assert (handtype lst1 = Pair(sort lst1));
  assert (handtype lst2 = Busted(sort lst2));
()

let handtype_3_test () = 
  let lst1 = [('c',6);('d',6);('s',6)] in
  let lst2 = [('c',2);('h',11);('s',8)] in
  let lst3 = [('d',6);('s',13);('s',6)] in
  assert (handtype lst1 = Three(sort lst1));
  assert (handtype lst2 = Busted(sort lst2));
  assert (handtype lst3 = Pair(sort lst3));
()

let handtype_4_test () = 
  let lst1 = [('c',6);('d',6);('s',6);('d',7)] in
  let lst2 = [('c',2);('h',11);('s',8);('d',14)] in
  let lst3 = [('d',6);('s',13);('s',6);('s',12)] in
  let lst4 = [('c',7);('d',7);('s',7);('d',7)] in
  let lst5 = [('d',6);('s',12);('s',6);('s',12)] in
  assert (handtype lst1 = Three(sort lst1));
  assert (handtype lst2 = Busted(sort lst2));
  assert (handtype lst3 = Pair(sort lst3));
  assert (handtype lst4 = Four(sort lst4));
  assert (handtype lst5 = Two_Pair(sort lst5));
()

let handtype_5_test () = 
  let lst1 = [('c',5);('s',14);('h',10);('s',7);('s',9);('s',10);('s',3)] in 
  let lst2 = [('s',14);('h',10);('s',7);('c',5);('s',10);('s',9);('s',3)] in 
  let lst3 = [('c',5);('s',5);('d',5);('h',5); ('c',14)] in
  let lst4 = [('s',5);('h',5);('c',3);('d',5); ('s',14)] in 
  let lst5 = [('s',10);('c',5);('s',11);('s',12);('h',10);('s',13);('s',14)] in
  let lst6 = [('s',5);('h',3);('c',5);('d',3);('s',3)] in
  let lst7 = [('s',10);('c',5);('d',11);('s',12);('h',10);('s',13);('s',14)] in
  let lst8 = [('s',10);('c',10);('d',11);('s',2);('h',11);('s',13);('s',14)] in
  let lst9 = [('s',10);('c',5);('d',4);('s',12);('h',10);('s',13);('s',14)] in
  let lst10 = [('s',10);('c',5);('h',6);('s',12);('h',3);('h',13);('s',14)] in
  let lst11 = [('s',10);('s',5);('c',11);('s',12);('h',10);('s',13);('s',14)] in
  let lst12 = [('s',10);('c',11);('h',11);('s',12);('h',10);('s',13);
    ('s',14);('d',11);('s',2)] in
  assert(handtype lst1 = Flush(sort lst1));
  assert(handtype lst2 = Flush(sort lst2));
  assert(handtype lst3 = Four(sort lst3));
  assert(handtype lst4 = Three(sort lst4));
  assert(handtype lst5 = Straight_Flush(sort lst5));
  assert(handtype lst6 = Full_House(sort lst6));
  assert(handtype lst7 = Straight(sort lst7));
  assert(handtype lst8 = Two_Pair(sort lst8));
  assert(handtype lst9 = Pair(sort lst9));
  assert(handtype lst10 = Busted(sort lst10)); 
  assert(handtype lst11 = Flush(sort lst11));
  assert(handtype lst12 = Full_House(sort lst12));
()

let handtype_test () =
  handtype_2_test ();
  handtype_3_test ();
  handtype_4_test ();
  handtype_5_test ();
()

let full_abstraction_rank_test () = 
  assert(get_rank_hand [('d',2);('c',8);('c',11);('d',11);('d',12)] = 22);
  assert(get_rank_hand [('d',2);('d',4);('d',5);('h',8);('s',13)] = 34);
  assert(get_rank_hand [('c',7);('h',8);('h',10);('c',12);('s',13)] = 36);
  assert(get_rank_hand [('c',5);('s',5);('d',8);('h',9);('h',12)] = 28);
  assert(get_rank_hand [('d',8);('d',13);('s',13);('d',14);('s',14)] = 9);
  assert(get_rank_hand [('d',3);('s',8);('d',14);('h',14);('s',14)] = 7);
  assert(get_rank_hand [('h',2);('s',11);('c',12);('h',12);('h',14)] = 18);
  assert(get_rank_hand [('c',3);('d',7);('h',9);('c',11);('h',14)] = 32);
  assert(get_rank_hand [('h',4);('c',5);('s',8);('c',10);('s',10)] = 24);
  assert(get_rank_hand [('c',8);('c',9);('d',10);('h',10);('s',12)] = 23);
  assert(get_rank_hand [('s',4);('s',6);('h',8);('d',9);('d',11)] = 40);
  assert(get_rank_hand [('h',4);('c',6);('s',7);('c',8);('h',10)] = 39);
  assert(get_rank_hand [('d',8);('d',9);('c',10);('d',11);('c',12)] = 6);
  assert(get_rank_hand [('s',2);('h',8);('c',9);('d',14);('h',14)] = 16);
  assert(get_rank_hand [('s',4);('d',6);('c',10);('c',12);('d',12)] = 20);
  assert(get_rank_hand [('s',3);('d',4);('s',5);('c',7);('h',13)] = 35);
  assert(get_rank_hand [('h',4);('c',7);('h',7);('h',8);('d',9)] = 25);
  assert(get_rank_hand [('h',3);('c',5);('h',12);('c',13);('s',14)] = 31);
  assert(get_rank_hand [('h',2);('c',9);('d',9);('s',9);('c',12)] = 8);
  assert(get_rank_hand [('h',2);('d',3);('c',4);('d',4);('c',8)] = 27);
  assert(get_rank_hand [('d',5);('d',8);('h',8);('d',10);('c',14)] = 26);
  assert(get_rank_hand [('h',2);('d',6);('s',9);('h',11);('h',14)] = 30);
  assert(get_rank_hand [('d',4);('c',5);('c',8);('c',9);('d',11)] = 38);
  assert(get_rank_hand [('d',2);('c',4);('d',5);('h',7);('d',8)] = 42);
  assert(get_rank_hand [('c',4);('h',4);('d',10);('s',10);('s',13)] = 12);
  assert(get_rank_hand [('s',4);('h',7);('c',11);('s',12);('s',13)] = 33);
  assert(get_rank_hand [('c',3);('h',4);('s',5);('s',8);('s',11)] = 37);
  assert(get_rank_hand [('c',6);('c',10);('c',11);('c',12);('c',13)] = 5);
  assert(get_rank_hand [('c',8);('c',9);('c',10);('c',11);('c',12)] = 1);
  assert(get_rank_hand [('d',2);('h',11);('s',11);('d',14);('s',14)] = 10);
  assert(get_rank_hand [('d',4);('s',10);('d',13);('d',14);('s',14)] = 14);
  assert(get_rank_hand [('h',8);('h',9);('h',10);('s',11);('d',14)] = 29);
  assert(get_rank_hand [('d',2);('h',2);('s',2);('c',7);('d',7)] = 4);
  assert(get_rank_hand [('d',8);('c',11);('d',12);('h',13);('s',13)] = 19);
  assert(get_rank_hand [('d',4);('s',4);('d',6);('h',6);('d',10)] = 11);
  assert(get_rank_hand [('h',4);('d',11);('h',12);('c',13);('h',13)] = 17);
  assert(get_rank_hand [('d',8);('d',12);('h',13);('c',14);('s',14)] = 15);
  assert(get_rank_hand [('h',6);('c',11);('h',11);('h',12);('h',13)] = 21);
  assert(get_rank_hand [('s',2);('d',4);('d',5);('s',6);('d',7)] = 41);
  assert(get_rank_hand [('c',3);('h',5);('d',6);('d',7);('c',8)] = 43);
  assert(get_rank_hand [('h',4);('c',12);('c',13);('c',14);('d',14)] = 13);
  assert(get_rank_hand [('s',10);('c',12);('d',12);('h',12);('s',12)] = 2);
  assert(get_rank_hand [('s',2);('s',3);('h',5);('d',7);('h',8)] = 44);
  assert(get_rank_hand [('c',5);('c',10);('d',10);('h',10);('s',10)] = 3);
  assert(get_rank_hand [('c',2);('s',2);('s',3);('h',5);('d',9)] = 28);
  assert(get_rank_hand [('h',8);('h',10);('c',11);('s',12);('h',14)] = 29);
  assert(get_rank_hand [('h',3);('s',8);('s',9);('d',11);('c',13)] = 36);
  assert(get_rank_hand [('s',2);('s',8);('c',10);('s',12);('d',14)] = 30);
  assert(get_rank_hand [('h',3);('c',5);('d',9);('h',9);('s',9)] = 8);
  assert(get_rank_hand [('d',11);('s',11);('d',13);('d',14);('s',14)] = 9);
  assert(get_rank_hand [('s',3);('c',7);('h',7);('h',9);('h',12)] = 26);
  assert(get_rank_hand [('s',3);('s',7);('h',12);('c',13);('d',14)] = 31);
  assert(get_rank_hand [('h',8);('d',10);('s',10);('s',11);('s',14)] = 22);
  assert(get_rank_hand [('c',8);('s',9);('h',10);('h',13);('s',13)] = 19);
  assert(get_rank_hand [('d',7);('s',9);('s',13);('d',14);('h',14)] = 16);
  assert(get_rank_hand [('s',4);('h',7);('d',9);('s',10);('c',14)] = 32);
  assert(get_rank_hand [('d',4);('c',10);('h',10);('h',12);('c',14)] = 24);
  assert(get_rank_hand [('c',3);('c',4);('h',4);('c',5);('c',6)] = 25);
  assert(get_rank_hand [('c',2);('h',7);('c',8);('d',9);('s',12)] = 35);
  assert(get_rank_hand [('s',3);('s',6);('s',7);('s',9);('s',13)] = 5);
  assert(get_rank_hand [('s',9);('s',10);('s',11);('s',12);('s',13)] = 1);
  assert(get_rank_hand [('d',4);('s',6);('s',7);('s',10);('h',11)] = 38);
  assert(get_rank_hand [('d',2);('h',2);('c',6);('s',6);('d',10)] = 12);
  assert(get_rank_hand [('c',2);('h',3);('h',7);('d',13);('h',13)] = 18);
  assert(get_rank_hand [('c',7);('c',12);('d',13);('h',13);('c',14)] = 17);
  assert(get_rank_hand [('c',3);('d',5);('s',11);('d',12);('d',13)] = 33);
  assert(get_rank_hand [('h',5);('c',7);('d',7);('c',14);('d',14)] = 10);
  assert(get_rank_hand [('s',2);('h',10);('c',11);('d',11);('s',11)] = 7);
  assert(get_rank_hand [('d',6);('h',9);('c',10);('d',13);('h',13)] = 20);
  assert(get_rank_hand [('h',2);('s',3);('s',6);('h',8);('h',13)] = 34);
  assert(get_rank_hand [('s',2);('d',3);('c',4);('h',7);('c',8)] = 43);
  assert(get_rank_hand [('s',4);('h',7);('h',8);('d',10);('d',11)] = 40);
  assert(get_rank_hand [('d',3);('d',4);('c',5);('c',6);('d',7)] = 6);
  assert(get_rank_hand [('h',2);('h',7);('d',8);('d',9);('s',11)] = 39);
  assert(get_rank_hand [('s',2);('d',4);('h',5);('s',7);('h',8)] = 44);
  assert(get_rank_hand [('c',4);('c',9);('h',9);('s',10);('s',11)] = 23);
  assert(get_rank_hand [('d',3);('d',7);('d',8);('c',9);('s',9)] = 21);
  assert(get_rank_hand [('c',6);('s',6);('s',7);('d',8);('h',14)] = 27);
  assert(get_rank_hand [('s',2);('s',7);('c',8);('h',9);('s',10)] = 37);
  assert(get_rank_hand [('s',10);('h',12);('s',13);('c',14);('s',14)] = 13);
  assert(get_rank_hand [('s',3);('s',7);('s',10);('c',14);('s',14)] = 14);
  assert(get_rank_hand [('d',2);('h',3);('d',4);('d',5);('s',7)] = 41);
  assert(get_rank_hand [('d',6);('h',6);('s',6);('c',10);('s',10)] = 4);
  assert(get_rank_hand [('c',3);('d',3);('d',8);('h',8);('d',14)] = 11);
  assert(get_rank_hand [('c',8);('c',9);('s',10);('h',14);('s',14)] = 15);
  assert(get_rank_hand [('h',2);('h',3);('s',5);('h',7);('s',8)] = 42);
  assert(get_rank_hand [('c',2);('d',2);('h',2);('s',2);('h',4)] = 3);
  assert(get_rank_hand [('c',11);('d',11);('h',11);('s',11);('s',13)] = 2);
()

let compare_hands_test () = 
  assert(compare_hands 
    [('c',2);('s',2);('s',3);('h',5);('d',9);('c',11);('h',14)] 
    [('h',2);('c',4);('c',7);('d',7);('d',8);('d',9);('c',10)]= -1);
  assert(compare_hands 
    [('h',8);('s',8);('s',9);('h',10);('d',11);('s',12);('c',13)] 
    [('d',2);('c',4);('d',5);('h',5);('h',6);('c',7);('d',8)] = 1);
  assert(compare_hands 
    [('h',3);('h',6);('s',8);('c',9);('c',10);('c',12);('s',13)] 
    [('c',2);('d',3);('h',4);('d',6);('s',10);('c',11);('s',13)] = 1);
  assert(compare_hands  
    [('s',2);('h',3);('d',9);('h',9);('s',9);('s',12);('d',14)] 
    [('c',4);('h',4);('s',4);('d',5);('d',6);('h',7);('h',13)] = 1);
  assert(compare_hands  
    [('c',5);('c',7);('d',11);('s',11);('d',13);('d',14);('s',14)] 
    [('d',5);('h',6);('d',7);('d',8);('s',8);('d',13);('h',13)] = 1);
  assert(compare_hands  
    [('d',2);('c',5);('c',7);('d',8);('s',8);('d',14);('s',14)] 
    [('d',5);('h',6);('d',7);('d',8);('s',8);('d',14);('h',14)] = 0);
  assert(compare_hands  
    [('d',2);('c',5);('c',6);('d',6);('s',8);('d',14);('s',14)] 
    [('d',5);('h',6);('d',7);('d',8);('s',8);('d',14);('h',14)] = -1);
  assert(compare_hands  
    [('h',6);('c',8);('h',10);('h',11);('h',13);('s',13);('h',14)] 
    [('s',2);('s',7);('c',9);('s',9);('s',10);('h',12);('s',13)] = 1);
  assert(compare_hands  
    [('s',4);('s',5);('d',7);('s',8);('d',9);('s',10);('c',14)] 
    [('s',2);('d',4);('s',7);('c',8);('h',9);('d',13);('c',14)] = -1);
  assert(compare_hands  
    [('s',3);('d',4);('h',7);('c',10);('h',10);('h',12);('c',14)] 
    [('c',5);('d',7);('d',8);('c',9);('h',11);('s',11);('s',12)] = -1);
  assert(compare_hands  
    [('s',3);('d',4);('h',7);('c',11);('h',11);('h',12);('c',14)] 
    [('c',5);('d',7);('d',8);('c',9);('h',11);('s',11);('s',12)] = 1);
  assert(compare_hands  
    [('s',3);('s',6);('h',7);('s',7);('d',9);('s',9);('s',13)] 
    [('c',3);('h',5);('s',5);('c',8);('d',8);('d',9);('d',13)] = 1);
  assert(compare_hands  
    [('s',3);('s',6);('h',7);('d',7);('d',9);('s',9);('s',13)] 
    [('c',3);('d',5);('h',5);('c',8);('d',9);('d',9);('d',13)] = 1);
  assert(compare_hands  
    [('s',3);('s',6);('h',7);('s',7);('d',9);('d',9);('s',11)] 
    [('c',3);('h',5);('s',5);('c',8);('d',9);('d',9);('d',10)] = 1);
  assert(compare_hands  
    [('d',2);('d',4);('s',4);('c',6);('d',6);('s',6);('s',7)] 
    [('h',2);('s',2);('c',5);('c',8);('d',8);('s',8);('d',9)] = -1);
  assert(compare_hands  
    [('d',2);('d',4);('s',4);('c',8);('d',8);('s',8);('s',9)] 
    [('h',2);('s',2);('c',5);('c',8);('d',8);('s',8);('d',9)] = 1);
  assert(compare_hands  
    [('s',2);('h',3);('h',7);('d',12);('s',12);('h',13);('s',14)]
    [('s',3);('c',7);('h',8);('c',12);('d',12);('d',13);('c',14)] = -1);
  assert(compare_hands  
    [('d',2);('h',4);('h',5);('h',6);('d',7);('c',14);('d',14)] 
    [('c',2);('d',4);('c',6);('h',9);('h',10);('d',14);('h',14)] = -1);
  assert(compare_hands  
    [('s',2);('h',10);('c',11);('d',11);('s',11);('h',13);('d',14)]
    [('s',3);('d',6);('h',7);('c',12);('d',14);('h',14);('s',14)] = -1);
  assert(compare_hands  
    [('s',2);('h',10);('c',11);('d',11);('s',11);('h',13);('d',14)]
    [('s',3);('d',6);('h',7);('c',10);('d',11);('h',11);('s',11)] = 1);
  assert(compare_hands  
    [('d',2);('s',3);('h',4);('h',6);('s',7);('c',8);('d',10)] 
    [('d',2);('d',3);('h',4);('s',5);('d',7);('s',9);('h',10)] = -1);
  assert(compare_hands  
    [('c',3);('c',5);('c',7);('d',7);('h',7);('s',7);('c',10)] 
    [('d',4);('c',7);('d',7);('h',7);('s',7);('c',9);('c',13)] = -1);
  assert(compare_hands  
    [('c',3);('c',5);('c',6);('d',6);('h',6);('s',6);('c',10)] 
    [('d',4);('c',7);('d',7);('h',7);('s',7);('c',9);('c',13)] = -1);
  assert(compare_hands  
    [('c',5);('s',6);('c',7);('c',14);('d',14);('h',14);('s',14)] 
    [('s',7);('c',11);('d',11);('h',11);('s',11);('h',13);('s',13)] = 1);
  assert(compare_hands  
    [('h',3);('d',4);('d',10);('d',11);('d',12);('d',13);('d',14)] 
    [('h',4);('d',5);('h',10);('h',11);('h',12);('h',13);('h',14)] = 0);
  assert(compare_hands  
    [('h',3);('d',4);('d',9);('d',10);('d',11);('d',12);('d',13)] 
    [('h',4);('d',5);('h',10);('h',11);('h',12);('h',13);('h',14)] = -1);
()

(* all tests *)
let run_tests () = 
  test_suit_order ();
  card_compare_test ();
  card_compare_no_suit_test ();
  nums_same_test ();
  handtype_test (); 
  full_abstraction_rank_test ();
  compare_hands_test ();
()
  
end
   
(*let _ = PokerEval.run_tests ();;*)

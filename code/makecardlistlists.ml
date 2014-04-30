let rand : (int->int)= Random.self_init () ; Random.int

let gen_crd () = 
  (match rand 4 with 
   | 0 -> 'C'
   | 1 -> 'S'
   | 2 -> 'H'
   | 3 -> 'D' 
   | _ -> raise (Failure "rand returning greater than four: suitegen")), 
  ((rand 13) + 2)


let cardlist_gen (len:int) : (char*int) list = 
  let rec genr len acc = 
    match len with
    | 0 -> acc
    | _ -> genr (len-1) ((gen_crd())::acc) in
  if len < 0 then raise (Failure "cannot make list with negative length")
  else genr len []

(*****THE FUNCTIONS YOU ACTUALLY WILL NEED TO USE*****)
(* cardlists_gen accepts a list of integer lengths of the output lists, 
since shanyi wasn't specific about how this was wanted. 
to make a list of length 6 of lists all of length 4, do 
cardlists_gen (list_incr 4 0 6). 
To do a list of length 20 where each sublist is 3 shorter than the previous
one, starting at 100, do 
cardlists_gen (list_incr 100 (-3) 20) 
*)

let cardlists_gen (pattern:int list) : (char*int) list list = 
  List.map (fun a -> cardlist_gen a) pattern

let list_incr (start:int) (inc:int) (len:int) : int list= 
  let rec makr s i l acc = 
    match l with 
    | 0 -> acc
    | _ -> makr (s + i) i (l - 1) (s::acc) in
  List.rev (makr start inc len [])


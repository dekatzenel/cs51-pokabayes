type hand_prob_val = 
    {busted:float ; pair:float ; two_pair:float ; three:float ; straight:float ;
     flush:float ; full_house:float ; four:float ; straight_flush:float}


let round1 = BruteForce.get_vals 1000000 2 
let round2 = BruteForce.get_vals 1000000 5
let round3 = BruteForce.get_vals 1000000 6
let round4 = BruteForce.get_vals 1000000 7

let transform rnd vals = 
  let rec walker ht lst acc = 
    match ht, lst with 
    | 1, hd::tl -> walker (ht+1) tl {acc with straight_flush=hd}
    | 2, hd::tl -> walker (ht+1) tl {acc with four=hd}
    | 3, hd::tl -> walker (ht+1) tl {acc with full_house=hd}
    | 4, hd::tl -> walker (ht+1) tl {acc with flush=hd}
    | 5, hd::tl -> walker (ht+1) tl {acc with straight=hd}
    | 6, hd::tl -> walker (ht+1) tl {acc with three=hd}
    | 7, hd::tl -> walker (ht+1) tl {acc with two_pair=hd}
    | 8, hd::tl -> walker (ht+1) tl {acc with pair=hd}
    | 9, hd::tl -> walker (ht+1) tl {acc with busted=hd}
    | _, [] -> acc
    | _, _ -> raise (Failure "tranform list mismatch length thingy") in
  List.mapi 
    (fun i a -> 
      let blankrecord = 
        {busted=0.; pair=0.; two_pair=0.;
        three=0.;straight=0.;flush=0.;
        full_house=0.;four=0.;straight_flush=0.} in
      ([rnd; (i+1)], (walker 1 a blankrecord))) 
    vals

let rnd1 = transform 1 round1
let rnd2 = transform 2 round2
let rnd3 = transform 3 round3
let rnd4 = transform 4 round4

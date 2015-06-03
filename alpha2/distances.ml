open Batteries

let max_agg = List.fold_left max 0.

let euc_agg sez = 
    let square x = x ** 2. in
    sqrt (List.fold_left (+.) 0. (List.map square sez))
    
let avg_agg sez =
    let n = float (List.length sez) in
    (List.fold_left (+.) 0. sez) /. n
    
let med_agg sez = 
    let n = List.length sez in
    let sorted = List.sort compare sez in
    if (n mod 2 = 0) 
      then 0.5 *. ( List.nth sorted (n/2-1) +. List.nth sorted (n/2))
      else List.nth sorted (n/2)
      
let geo_agg sez = 
    let n = float_of_int (List.length sez) in
    let f x y = x*.y in
    let prod = List.fold_left f 1. sez in
    Batteries.Float.pow prod (1./.n)
    
let har_agg sez = 
    let n = float_of_int (List.length sez) in
    let f x y = x +. (1./.y) in
    let sum = List.fold_left f 0. sez in
    n /. sum
    
let wgh_agg ws sez =
    let n = List.fold_left (+.) 0. ws in
    let f x y = x*.y in
    let zip = List.map2 f ws sez in
    (List.fold_left (+.) 0. zip)/.n
    
let sampledist d s1 s2 = 1.

let rampa mini maks
    = fun x y -> min 1. ((max 0. (abs_float (x -. y) -. mini))/.(maks -. mini))

type distance =
    | Num of num_dist
    | Set of set_dist
    | Tup of tuple_dist
    | Dis of discrete_distance
    | Seq of sequence_dist
and num_dist =
    | Rampa of float*float
    | NDelta
and discrete_distance =
    | DDelta
and tuple_dist =
    | Max of distance list
    | Euc of distance list
    | Avg of distance list
    | Med of distance list
    | Har of distance list
    | Geo of distance list
    | Wgh of float list*distance list
and set_dist =
    | SetSample of distance
and sequence_dist =
    | SeqSample of distance
    
type data =
    | Discrete of string
    | Numerical of float
    | Set of data list
    | Tuple of data list
    | Sequence of data list
    | Missing

let rec calculate dist d1 d2 = 
    let pomo fs xs ys = 
        let apply f x = f x in
        List.map2 apply (List.map2 apply (List.map calculate fs) xs) ys
    in
    match dist,d1,d2 with
        | _,Missing,_ -> 1.
        | _,_,Missing -> 1.
        | Num Rampa (a,b), Numerical x, Numerical y -> rampa a b x y
        | Num NDelta, Numerical x, Numerical y -> if x = y then 0. else 1.
        | Dis DDelta, Discrete x, Discrete y -> if x = y then 0. else 1.
        | Tup Max lst, Tuple x, Tuple y -> max_agg (pomo lst x y)
        | Tup Euc lst, Tuple x, Tuple y -> euc_agg (pomo lst x y)
        | Tup Avg lst, Tuple x, Tuple y -> avg_agg (pomo lst x y)
        | Tup Med lst, Tuple x, Tuple y -> med_agg (pomo lst x y)
        | Tup Har lst, Tuple x, Tuple y -> har_agg (pomo lst x y)
        | Tup Geo lst, Tuple x, Tuple y -> geo_agg (pomo lst x y)
        | Tup Wgh (ws,lst), Tuple x, Tuple y -> wgh_agg ws (pomo lst x y)
        | Set SetSample f, Set x, Set y -> sampledist (calculate f) x y
        | Seq SeqSample f, Sequence x, Sequence y -> sampledist (calculate f) x y
        | _,_,_ -> raise (Failure "Neznana razdalja ali neujemajoc podatek.") 
    
let distance_repr d = 
    let weights_repr lst =
        let sez = List.map (string_of_float) lst in
        String.concat " " (["["] @ sez @ ["]"])
    in
    let acc = ref [] in
    let rec repr n d =
        match d with
            | Num Rampa (a,b) -> acc := (Printf.sprintf "%sRampa (%F,%F)" (Bytes.make n ' ') a b)::!acc
            | Num NDelta -> acc := (Printf.sprintf "%sNDelta" (Bytes.make n ' '))::!acc
            | Dis DDelta -> acc := (Printf.sprintf "%sDDelta" (Bytes.make n ' '))::!acc
            | Tup Max lst -> acc := (Printf.sprintf "%sMax" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | Tup Euc lst -> acc := (Printf.sprintf "%sEuc" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | Tup Avg lst -> acc := (Printf.sprintf "%sAvg" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | Tup Med lst -> acc := (Printf.sprintf "%sMed" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | Tup Har lst -> acc := (Printf.sprintf "%sHar" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | Tup Geo lst -> acc := (Printf.sprintf "%sGeo" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | Tup Wgh (ws,lst) -> acc := (Printf.sprintf "%sWgh%s" (Bytes.make n ' ') (weights_repr ws))::!acc; List.map (repr (n+4)) lst; ()
            | Set SetSample f -> acc := (Printf.sprintf "%sSetSample" (Bytes.make n ' '))::!acc; repr (n+4) f
            | Seq SeqSample f -> acc := (Printf.sprintf "%sSeqSample" (Bytes.make n ' '))::!acc; repr (n+4) f
    in 
    repr 0 d;
    let temp = List.rev !acc in
    String.concat "\n" temp
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
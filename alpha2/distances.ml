type data =
    | Discrete of string
    | Numerical of float
    | Set of data list
    | Tuple of data list
    | Sequence of data list
    | Missing

type schema =
    | N of float*float
    | D of string list
    | Tp of schema list
    | St of schema
    | Sq of schema

type distance =
    | NumD of num_dist
    | SetD of set_dist
    | TupD of tuple_dist
    | DisD of discrete_distance
    | SeqD of sequence_dist
and num_dist =
    | Rampa of float*float
and discrete_distance =
    | Delta
    | Matrix of (string*string*float) list
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

let rec matrix_distance lst s1 s2 = match lst with
    | [] -> raise (Failure "Podatka ni v matriki")
    | x :: xs -> match x with
                    | (a1,a2,f) -> if ((a1,a2) = (s1,s2) || (a1,a2) = (s2,s1)) then f else matrix_distance xs s1 s2
                    | _ -> raise (Failure "Narobe podana 'matrika'")

let rec calculate dist d1 d2 =
    let pomo fs xs ys =
        let apply f x = f x in
        List.map2 apply (List.map2 apply (List.map calculate fs) xs) ys
    in
    match dist,d1,d2 with
        | _,Missing,_ -> 1.
        | _,_,Missing -> 1.
        | NumD Rampa (a,b), Numerical x, Numerical y -> rampa a b x y
        | DisD Delta, Discrete x, Discrete y -> if x = y then 0. else 1.
        | DisD Matrix lst, Discrete x, Discrete y -> matrix_distance lst x y
        | TupD Max lst, Tuple x, Tuple y -> max_agg (pomo lst x y)
        | TupD Euc lst, Tuple x, Tuple y -> euc_agg (pomo lst x y)
        | TupD Avg lst, Tuple x, Tuple y -> avg_agg (pomo lst x y)
        | TupD Med lst, Tuple x, Tuple y -> med_agg (pomo lst x y)
        | TupD Har lst, Tuple x, Tuple y -> har_agg (pomo lst x y)
        | TupD Geo lst, Tuple x, Tuple y -> geo_agg (pomo lst x y)
        | TupD Wgh (ws,lst), Tuple x, Tuple y -> wgh_agg ws (pomo lst x y)
        | SetD SetSample f, Set x, Set y -> sampledist (calculate f) x y
        | SeqD SeqSample f, Sequence x, Sequence y -> sampledist (calculate f) x y
        | _,_,_ -> raise (Failure "Neznana razdalja ali neujemajoc podatek.")

let distance_repr d =
    let weights_repr lst =
        let sez = List.map (string_of_float) lst in
        String.concat " " (["["] @ sez @ ["]"])
    in
    let matrix_repr lst =
        let pomo (a,b,f) = Printf.sprintf "(%s, %s, %F)" a b f in
        let sez = List.map pomo lst in
        String.concat " " (["["] @ sez @ ["]"])
    in
    let acc = ref [] in
    let rec repr n d =
        match d with
            | NumD Rampa (a,b) -> acc := (Printf.sprintf "%sRampa (%F,%F)" (Bytes.make n ' ') a b)::!acc
            | DisD Delta -> acc := (Printf.sprintf "%sDelta" (Bytes.make n ' '))::!acc
            | DisD Matrix lst -> acc := (Printf.sprintf "%sMatrix %s" (Bytes.make n ' ') (matrix_repr lst))::!acc
            | TupD Max lst -> acc := (Printf.sprintf "%sMax" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | TupD Euc lst -> acc := (Printf.sprintf "%sEuc" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | TupD Avg lst -> acc := (Printf.sprintf "%sAvg" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | TupD Med lst -> acc := (Printf.sprintf "%sMed" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | TupD Har lst -> acc := (Printf.sprintf "%sHar" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | TupD Geo lst -> acc := (Printf.sprintf "%sGeo" (Bytes.make n ' '))::!acc; List.map (repr (n+4)) lst; ()
            | TupD Wgh (ws,lst) -> acc := (Printf.sprintf "%sWgh %s" (Bytes.make n ' ') (weights_repr ws))::!acc; List.map (repr (n+4)) lst; ()
            | SetD SetSample f -> acc := (Printf.sprintf "%sSetSample" (Bytes.make n ' '))::!acc; repr (n+4) f
            | SeqD SeqSample f -> acc := (Printf.sprintf "%sSeqSample" (Bytes.make n ' '))::!acc; repr (n+4) f
    in
    repr 0 d;
    let temp = List.rev !acc in
    String.concat "\n" temp

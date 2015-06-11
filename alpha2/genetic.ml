open Reader
open Printf
open Distances

let rec select key k lst = match lst with
    | [] -> []
    | x :: xs -> let ys, zs = List.partition (key x) xs in
                let l = List.length ys in
                if k < l then
                    select key k ys
                else if k > l then
                    (x::ys) @ (select key (k-l-1) zs)
                else
                    ys

let kneighs dista distc k mean tset sample =
    let pomo x = (min (1./.(calculate dista sample (fst x))) 999999999., x) in
    let mapped = List.map pomo tset in
    let key x y = (fst x) < (fst y) in
    let neighs = select key k mapped in
    mean dista distc neighs sample

let medoid da dc set sample =
    let f x = (snd (snd x), fst x) in
    let temp = List.map f set in
    let rec count (a,b) xs = match xs with
        | [] -> (a,b,[])
        | (c,d) :: sez ->
            let (x,y,z) = count (a,b) sez in
            if a = c
            then (a,y+.b+.d,z)
            else (a,y+.b,(c,d)::z)
    in
    let rec combine lst = match lst with
        | [] -> []
        | l::ls -> let (a,b,c) = count l ls in (a,b) :: (combine c)
    in
    let temp2 = combine temp in
    let pomo (a,b) (c,d) = if b > d then (a,b) else (c,d) in
    fst (List.fold_left pomo ((Set []),0.) temp2)

let random_tuple lst =
    let n = List.length lst in
    let a = Random.int 7 in
    if a = 0 then TupD (Max lst)
    else if a = 1 then TupD (Euc lst)
    else if a = 2 then TupD (Avg lst)
    else if a = 3 then TupD (Med lst)
    else if a = 4 then TupD (Har lst)
    else if a = 5 then TupD (Geo lst)
    else TupD (Wgh (List.map Random.float (Batteries.List.make n 1.), lst))

let random_discrete lst =
    if (Random.float 1.) < 0.
    then DisD Delta
    else
        let pomo x y = (x,y,1.) in
        let rec kombinacije s1 s2 = match s2 with
            | [] -> []
            | x :: xs -> [(x,x,0.)] @ (List.map (pomo x) s1) @ (kombinacije (x::s1) xs)
        in DisD (Matrix (List.sort Pervasives.compare (kombinacije [] lst)))

let rec mate dist1 dist2 =
    let par = if Random.bool () then (dist1,dist2) else (dist2,dist1) in
    match par with
    | NumD Rampa (a,b), _ -> NumD (Rampa (a,b))
    | DisD Delta, _ -> DisD Delta
    | _, DisD Delta -> DisD Delta
    | DisD Matrix lst1, DisD Matrix lst2 -> DisD (Matrix (List.map2 (fun (a,b,f) (c,d,g) -> (a,b,(f+.g)/.2.)) lst1 lst2))
    | SetD SetSample d1, SetD SetSample d2 -> SetD (SetSample (mate d1 d2))
    | SeqD SeqSample d1, SeqD SeqSample d2 -> SeqD (SeqSample (mate d1 d2))
    | TupD Max lst1, (TupD Euc lst2 | TupD Avg lst2 | TupD Max lst2 | TupD Med lst2 | TupD Har lst2 | TupD Geo lst2) -> TupD (Max (List.map2 mate lst1 lst2))
    | TupD Euc lst1, (TupD Euc lst2 | TupD Avg lst2 | TupD Max lst2 | TupD Med lst2 | TupD Har lst2 | TupD Geo lst2) -> TupD (Euc (List.map2 mate lst1 lst2))
    | TupD Avg lst1, (TupD Euc lst2 | TupD Avg lst2 | TupD Max lst2 | TupD Med lst2 | TupD Har lst2 | TupD Geo lst2) -> TupD (Avg (List.map2 mate lst1 lst2))
    | TupD Med lst1, (TupD Euc lst2 | TupD Avg lst2 | TupD Max lst2 | TupD Med lst2 | TupD Har lst2 | TupD Geo lst2) -> TupD (Med (List.map2 mate lst1 lst2))
    | TupD Har lst1, (TupD Euc lst2 | TupD Avg lst2 | TupD Max lst2 | TupD Med lst2 | TupD Har lst2 | TupD Geo lst2) -> TupD (Har (List.map2 mate lst1 lst2))
    | TupD Geo lst1, (TupD Euc lst2 | TupD Avg lst2 | TupD Max lst2 | TupD Med lst2 | TupD Har lst2 | TupD Geo lst2) -> TupD (Geo (List.map2 mate lst1 lst2))
    | TupD Max lst1, TupD Wgh (ws, lst2) -> TupD (Max (List.map2 mate lst1 lst2))
    | TupD Euc lst1, TupD Wgh (ws, lst2) -> TupD (Euc (List.map2 mate lst1 lst2))
    | TupD Avg lst1, TupD Wgh (ws, lst2) -> TupD (Avg (List.map2 mate lst1 lst2))
    | TupD Med lst1, TupD Wgh (ws, lst2) -> TupD (Med (List.map2 mate lst1 lst2))
    | TupD Har lst1, TupD Wgh (ws, lst2) -> TupD (Har (List.map2 mate lst1 lst2))
    | TupD Geo lst1, TupD Wgh (ws, lst2) -> TupD (Geo (List.map2 mate lst1 lst2))
    | TupD Wgh (ws, lst1), (TupD Euc lst2 | TupD Avg lst2 | TupD Max lst2 | TupD Med lst2 | TupD Har lst2 | TupD Geo lst2) -> TupD (Wgh (ws, List.map2 mate lst1 lst2))
    | TupD Wgh (ws1,lst1),TupD Wgh (ws2,lst2) -> TupD (Wgh (List.map2 (fun x y -> (x+.y)/.2.) ws1 ws2, List.map2 mate lst1 lst2))
    | _ -> raise (Failure "Starsa razlicnih tipov")

let rec mutate pow dist =
    match dist with
    | SetD SetSample d -> SetD (SetSample (mutate pow d))
    | SeqD SeqSample d -> SeqD (SeqSample (mutate pow d))
    | DisD Delta -> DisD Delta
    | DisD Matrix lst ->
        let pert (a,b,x) = (a,b,(min 1. (1. +. pow -. 2.*.(Random.float pow))*.x)) in
        DisD (Matrix (List.map pert lst))
    | NumD Rampa (a,b) -> NumD (Rampa (a*.(1.+.pow*.(1.-.2.*.Random.float 1.)),b*.(1.+.pow*.(1.-.2.*.Random.float 1.))))
    | TupD Max lst -> if (Random.float 1. < pow) then random_tuple (List.map (mutate pow) lst) else TupD (Max (List.map (mutate pow) lst))
    | TupD Euc lst -> if (Random.float 1. < pow) then random_tuple (List.map (mutate pow) lst) else TupD (Euc (List.map (mutate pow) lst))
    | TupD Avg lst -> if (Random.float 1. < pow) then random_tuple (List.map (mutate pow) lst) else TupD (Avg (List.map (mutate pow) lst))
    | TupD Med lst -> if (Random.float 1. < pow) then random_tuple (List.map (mutate pow) lst) else TupD (Med (List.map (mutate pow) lst))
    | TupD Har lst -> if (Random.float 1. < pow) then random_tuple (List.map (mutate pow) lst) else TupD (Har (List.map (mutate pow) lst))
    | TupD Geo lst -> if (Random.float 1. < pow) then random_tuple (List.map (mutate pow) lst) else TupD (Geo (List.map (mutate pow) lst))
    | TupD Wgh (ws, lst) -> if (Random.float 1. < pow)
        then random_tuple (List.map (mutate pow) lst)
        else let pert x = (1. +. pow -. 2.*.(Random.float pow))*.x in
        TupD (Wgh ((List.map pert ws),(List.map (mutate pow) lst)))

let rec random_dist shema = match shema with
    | N (a,b) -> NumD (Rampa (b*.Random.float 0.5,b/.2.+.b*.Random.float 1.))
    | D lst -> random_discrete lst
    | St s -> SetD (SetSample (random_dist s))
    | Sq s -> SeqD (SeqSample (random_dist s))
    | Tp ss -> random_tuple (List.map random_dist ss)

let offspring pow sh parents = match parents with
    | [a;b;c] -> [a; b; c; mutate pow (mate a b); mutate pow (mate a c); mutate pow (mate b c); random_dist sh; random_dist sh; random_dist sh]
    | _ -> raise (Failure "Prekratek seznam za zarod.")

let evaluate da dc podatki =
    let accuracy = ref 0. in
    let filter x = Random.bool () in
    (for j = 1 to 10 do
        let (train,test) = List.partition filter podatki in
        let knn x = kneighs da dc 13 medoid train (fst x) in
        let rezultati = List.map knn test in
        let resitve = List.map snd test in
        let add a b c = if a=b then c+.1. else c in
        let hits = List.fold_right2 add resitve rezultati 0. in
        let acc = hits/.(float_of_int (List.length test)) in
        accuracy := !accuracy +. acc
    done);
    !accuracy /. 10.

let evolution pow len ime =
    let (s1,s2,podatki) = read_arff ime in
    print_int (List.length podatki); print_newline ();
    (match s1 with
    | Tp sez -> print_int (List.length sez); print_newline ()
    | _ -> ());
    let dummy = random_dist s2 in
    let fathers = ref [random_dist s1; random_dist s1; random_dist s1] in
    for gen = 0 to len do
        let candidates = offspring pow s1 !fathers in
        let eval x = (evaluate x dummy podatki, x) in
        let results = List.rev (List.sort Pervasives.compare (List.map eval candidates)) in
        let a,b,c = (List.nth results 0),(List.nth results 1),(List.nth results 2) in
        print_string "Generation: "; print_int gen; print_newline ();
        print_float (fst a); print_string "  ";
        print_float (fst b); print_string "  ";
        print_float (fst c); print_newline (); print_newline ();
        fathers := [snd a; snd b; snd c]
    done;
    print_string (distance_repr (List.hd !fathers))

let _ =
    let name = Sys.argv.(1) in
    let gens = int_of_string Sys.argv.(2) in
    let pow = float_of_string Sys.argv.(3) in
    (printf "Datoteka %s\n" name);
    evolution pow gens name

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
    let pomo x = (min (1./.(calculate dista sample (fst x))) 999999., x) in
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

let mean da dc set sample =
    let sum = ref 0. in
    let f s x =
        let w = fst x in match snd (snd x) with
        | Numerical a -> sum := !sum +. w; s +. w *. a
        | _ -> raise (Failure "Not a number in mean")
    in
    let total = List.fold_left f 0. set in
    Numerical (total /. !sum)
    
let randomize stratify data = 
	let randomized = List.sort (fun x y -> 1 - Random.int 3) data in
	if stratify 
	then  List.sort (fun x y -> compare (snd x) (snd y)) randomized
	else randomized
    
let generate_sets data folds fold =
	let filter i x = 
		i := !i + 1;
		!i mod folds = fold
	in
	List.partition (filter (ref 0)) data
	
(*let update_schema s tset =
	let rec bounds d1 d2 = match d1,d2 with
		| Discrete a, Discrete b -> Discrete a
		| (Numerical a), Numerical b -> Numerical (a+.b)
		| Missing, x -> x
		| x, Missing -> x
		| Tuple lst1, Tuple lst2 -> Tuple (List.map2 add_data lst1 lst2)
		| Set lst1, Set lst2 -> *)

let evaluate folds cshema da dc k podatki = match cshema with
    | N _ -> begin
        let unpack dat = match dat with
            | Numerical a -> a
            | _ -> raise (Failure "Not a number in unpack.")
        in 
        let coef = ref (float_of_int folds) in
        let randomized = randomize false podatki in
        (for j = 1 to folds do
            let (train,test) = generate_sets randomized folds j in
            let knn x = kneighs da dc k mean train (fst x) in
            let rezultati = List.map (fun x -> unpack (knn x)) test in
            let resitve = List.map (fun x -> unpack (snd x)) test in
            let nn = float_of_int (List.length test) in
            let avg = (List.fold_left (+.) 0. resitve) /. nn in
            let s_tot = List.fold_left (fun s y -> s +. (avg -. y)**2.) 0. resitve in
            let s_res = List.fold_left2 (fun s y x -> s +. (x -. y)**2.) 0. resitve rezultati in
            coef := !coef -. (s_res/.s_tot) 
        done);
        !coef /. (float_of_int folds)
        end    
    | _ -> begin
        let accuracy = ref 0. in
        let randomized = randomize true podatki in
        (for j = 1 to folds do
            let (train,test) = generate_sets randomized folds j in
            let knn x = kneighs da dc k medoid train (fst x) in
            let rezultati = List.map knn test in
            let resitve = List.map snd test in
            let count a b c = if a=b then c+.1. else c in
            let hits = List.fold_right2 count resitve rezultati 0. in
            let acc = hits/.(float_of_int (List.length test)) in
            accuracy := !accuracy +. acc
        done);
        !accuracy /. (float_of_int folds)
        end
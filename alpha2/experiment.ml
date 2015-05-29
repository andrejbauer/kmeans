
open Frame
open Reader
open Printf

(* Malo testiranja *)
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
      
let rec take n lst = 
    if n = 0 
    then [] 
    else (List.hd lst) :: (take (n-1) (List.tl lst))

let all2all s1 s2 =
    let f x y = (y,x) in
    let pomo sez x = List.map (f x) sez in
    List.flatten (List.map (pomo s1) s2) 
    
let test data = 
    let pairs = [all2all] in
    let aggs = [avg_agg] in
    possible_distances pairs aggs data
    
(* testiranje kNN *)
let testiraj ime = 
    let (s1,s2,podatki) = read_arff ime in
    let razdaljeA = possible_distances [all2all] [avg_agg; max_agg;med_agg] s1 in 
    let razdaljeC = possible_distances [all2all] [avg_agg; max_agg] s2 in 
    let pari = all2all razdaljeA razdaljeC in
    let mean da dc set sample =
        let f x = (snd x, 1./. (calculate da ((fst x),sample))) in
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
    in
    (for i = 1 to (List.length pari) do
        let (da,dc) = List.nth pari (i-1) in
        let accuracy = ref 0. in
        let filter x = Random.bool () in
        (for j = 1 to 10 do
            let (train,test) = List.partition filter podatki in
            let knn x = kneighs da dc 7 mean train (fst x) in
            let rezultati = List.map knn test in
            let resitve = List.map snd test in
            let add a b c = if a=b then c+.1. else c in
            let hits = List.fold_right2 add resitve rezultati 0. in
            let acc = hits/.(float_of_int (List.length test)) in
            accuracy := !accuracy +. acc
        done);
        print_float (!accuracy /. 10.);
        print_newline ()
    done)

let () = 
    for i = 1 to Array.length Sys.argv - 1 do
        let ime = Sys.argv.(i) in
        (printf "Datoteka %s\n" ime);
        testiraj ime        
    done
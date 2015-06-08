open Reader
open Printf
open Distances

let all2all s1 s2 =
    let f x y = (y,x) in
    let pomo sez x = List.map (f x) sez in
    List.flatten (List.map (pomo s1) s2)

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

let mean da dc set sample =
    let sum = ref 0. in
    let f s x =
        let w = fst x in match snd (snd x) with
        | Numerical a -> sum := !sum +. w; s +. w *. a
        | _ -> raise (Failure "Not a number in mean")
    in
    (List.fold_left f 0. set) /. !sum

let calc_distances shema =
    let rec distances data = match data with
        | D _ -> [DisD DDelta]
        | N (a,b) -> [NumD (Rampa (a,b))]
        | St ss ->
            let dists = distances ss in
            let f lst dist = lst @ [SetD (SetSample dist)] in
            List.fold_left f [] dists
        | Sq ss ->
            let dists = distances ss in
            let f lst dist = lst @ [SeqD (SeqSample dist)] in
            List.fold_left f [] dists
        | Tp ss ->
            let lst = List.map distances ss in
            let rec combos xs = (match xs with
                | [] -> []
                | [z] ->
                    let f x = [x] in
                    List.map f z
                | (z::zs) ->
                    let temp = combos zs in
                    let temp2 = all2all z temp in
                    let f (b,bs) = b :: bs in
                    List.map f temp2)
            in
            let dists = (combos lst) in
            let f lst ds = lst @ [TupD (Max ds); TupD (Geo ds); TupD (Har ds); TupD (Avg ds)] in
            List.fold_left f [] dists
    in distances shema


(* testiranje kNN *)
let testiraj ime print_distance =
    let (s1,s2,podatki) = read_arff ime in
    print_int (List.length podatki); print_newline ();
    (match s1 with
    | Tp sez -> print_int (List.length sez); print_newline ()
    | _ -> ());
    let razdaljeA = calc_distances s1 in
    let razdaljeC = calc_distances s2 in
    let pari = all2all razdaljeA razdaljeC in
    match s2 with
    | N (_,_) ->
        (for i = 1 to (List.length pari) do
            let (da,dc) = List.nth pari (i-1) in
            let rel_error = ref 0. in
            let abs_error = ref 0. in
            let filter x = Random.bool () in
            (for j = 1 to 10 do
                let (train,test) = List.partition filter podatki in
                let knn x = kneighs da dc 7 mean train (fst x) in
                let rezultati = List.map knn test in
                let resitve = List.map snd test in
                let add1 a b c = match a with
                    | Numerical aa -> c +. (abs_float (aa -. b)) /. aa
                    | _ -> raise (Failure "Not a number.")
                in
                let add2 a b c = match a with
                    | Numerical aa -> c +. (abs_float (aa -. b))
                    | _ -> raise (Failure "Not a number.")
                in
                let erel = List.fold_right2 add1 resitve rezultati 0. in
                let eabs = List.fold_right2 add2 resitve rezultati 0. in
                let avg1 = eabs/.(float_of_int (List.length test)) in
                let avg2 = erel/.(float_of_int (List.length test)) in
                rel_error := !rel_error +. avg2;
                abs_error := !abs_error +. avg1
            done);
            if print_distance
                then (print_string (distance_repr da); print_string (distance_repr dc))
                else ();
            print_string "Relative error: ";
            print_float (!rel_error /. 10.);
            print_string "     Absolute error: ";
            print_float (!abs_error /. 10.);
            print_newline ()
        done)
    | _ ->
        (for i = 1 to (List.length pari) do
            let (da,dc) = List.nth pari (i-1) in
            let accuracy = ref 0. in
            let filter x = Random.bool () in
            (for j = 1 to 10 do
                let (train,test) = List.partition filter podatki in
                let knn x = kneighs da dc 7 medoid train (fst x) in
                let rezultati = List.map knn test in
                let resitve = List.map snd test in
                let add a b c = if a=b then c+.1. else c in
                let hits = List.fold_right2 add resitve rezultati 0. in
                let acc = hits/.(float_of_int (List.length test)) in
                accuracy := !accuracy +. acc
            done);
            if print_distance
                then (print_string (distance_repr da); print_string (distance_repr dc))
                else ();
            print_string "Accuracy: ";
            print_float (!accuracy /. 10.);
            print_newline ()
        done)

let () =
    for i = 1 to Array.length Sys.argv - 1 do
        let ime = Sys.argv.(i) in
        (printf "Datoteka %s\n" ime);
        testiraj ime false
    done

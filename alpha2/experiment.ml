open Reader
open Printf
open Distances
open Algorithms

let all2all s1 s2 =
    let f x y = (y,x) in
    let pomo sez x = List.map (f x) sez in
    List.flatten (List.map (pomo s1) s2)

(*izbira testnih razdalj*)
let calc_distances shema =
    let rec distances data = match data with
        | D _ -> [DisD Delta]
        | N b -> [NumD (Rampa (0.,b))]
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
            let f lst ds = lst @ [TupD (Max ds); TupD (Geo ds); TupD (Har ds); TupD (Avg ds); TupD (Euc ds)] in
            List.fold_left f [] dists
    in distances shema


(* testiranje kNN *)
let testiraj ime =
    let (s1,s2,podatki) = read_arff ime in
    print_int (List.length podatki); print_newline ();
    (match s1 with
    | Tp sez -> print_int (List.length sez); print_newline ()
    | _ -> ());
    let razdaljeA = calc_distances s1 in
    let razdaljeC = calc_distances s2 in
    let pari = all2all razdaljeA razdaljeC in
    for i=0 to (List.length pari)-1 do
        let (da,dc) = List.nth pari i in
        let score = evaluate 10 s2 da dc 1 podatki in
        print_string "Accuracy: ";
        print_float score;
        print_newline ()
    done

let _ =
    for i = 1 to Array.length Sys.argv - 1 do
        let ime = Sys.argv.(i) in
        (printf "Datoteka %s\n" ime);
        testiraj ime
    done

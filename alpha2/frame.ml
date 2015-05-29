
type aggregate = float list -> float
type ('a,'b) pairing = 'a list -> 'b list -> ('a * 'b) list

let rampa mini maks
    = fun x y -> min 1. ((max 0. (abs_float (x -. y) -. mini))/.(maks -. mini))

type data =
    | Discrete of string
    | Numerical of float
    | Set of data list
    | Tuple of data list
    | Sequence of data list
    | Missing

type distance =
    | NumDistance of float*float
    | DiscreteDistance
    | TupleDistance of distance list * aggregate
    | SetDistance of distance*aggregate*(data,data) pairing
    | SequenceDistance of distance*aggregate*(data,data) pairing

type schema =
    | N of float*float
    | D of string list
    | Tp of schema list
    | St of schema
    | Sq of schema

let rec calculate distance p = match p with
    | (Missing,_) -> 1.
    | (_, Missing) -> 1.
    | (Discrete x,Discrete y) ->
        (match distance with
            | DiscreteDistance -> if x=y then 0. else 1.
            | _ -> raise (Failure "Razdalja in podatek se ne ujemata."))
    | (Numerical x, Numerical y) ->
        (match distance with
            | NumDistance (a,b) -> rampa a b x y
            | _ -> raise (Failure "Razdalja in podatek se ne ujemata."))
    | (Tuple xs, Tuple ys) ->
        (match distance with
            | TupleDistance (ds,agg) ->
                let pairs = List.combine xs ys in
                let lst = List.map2 calculate ds pairs in
                agg lst
            | _ -> raise (Failure "Razdalja in podatek se ne ujemata."))
    | (Set xs, Set ys) ->
        (match distance with
            | SetDistance (d,agg,pair) ->
                let pairs = pair xs ys in
                let lst = List.map (calculate d) pairs in
                agg lst
            | _ -> raise (Failure "Razdalja in podatek se ne ujemata."))
    | (Sequence xs, Sequence ys) ->
        (match distance with
            | SequenceDistance (d,agg,pair) ->
                let pairs = pair xs ys in
                let lst = List.map (calculate d) pairs in
                agg lst
            | _ -> raise (Failure "Razdalja in podatek se ne ujemata."))
    | (_,_) -> raise (Failure "Par v razdalji je razlicnega tipa.")


let kneighs dista distc k mean tset sample =
    let key x y = Pervasives.compare (calculate dista (sample,(fst x))) (calculate dista (sample,(fst y))) in
    let rec take n lst = if n = 0 then [] else (List.hd lst) :: (take (n-1) (List.tl lst)) in
    let neighs = List.sort key tset in
    mean dista distc (take k neighs) sample


let all2all s1 s2 =
    let f x y = (y,x) in
    let pomo sez x = List.map (f x) sez in
    List.flatten (List.map (pomo s1) s2)

let possible_distances pairs aggs s =
    let rec distances data = match data with
        | D _ -> [DiscreteDistance]
        | N (a,b) -> [NumDistance (a,b); NumDistance (a/.2.,b/.2.)]
        | St ss ->
            let lst = distances ss in
            let temp = all2all lst aggs in
            let temp2 = all2all temp pairs in
            let f ((a,b),c) = SetDistance (a,b,c) in
            List.map f temp2
        | Sq ss ->
            let lst = distances ss in
            let temp = all2all lst aggs in
            let temp2 = all2all temp pairs in
            let f ((a,b),c) = SequenceDistance (a,b,c) in
            List.map f temp2
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
            let f (a,b) = TupleDistance (a,b) in
            List.map f (all2all dists aggs)
    in distances s

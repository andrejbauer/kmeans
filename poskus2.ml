type 'a distance = 'a -> 'a -> float
type aggregate = float list -> float
type pairing 'a 'b  = 'a list -> 'b list -> ('a * 'b) list

let rampa mini maks : float distance 
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
    
    
let test data = 
    let pairs = [all2all] in
    let aggs = [avg_agg] in
    possible_distances pairs aggs data

(* branje .arff datotek *)
let read_arff filename =  
    let loci sez = 
        let st = ref 0 in
        let n = List.length sez in
        let filtr x = (st := !st+1; !st < n) in
        let (a,b) = List.partition filtr sez in
        (a,List.hd b)
        in
    let data = Csv.load filename in
    let n = List.length data in
    let parserji = ref [] in
    let numpar mini maksi ind str = 
        let num = float_of_string str in
        (Array.set !mini ind (min num (Array.get !mini ind)));
        (Array.set !maksi ind (max num (Array.get !maksi ind)));
        Numerical num 
        in
    let strpar str = Discrete str in
    let clean = ref [] in
    let mini = ref [||] in
    let maksi = ref [||] in 
    let attr = ref 0 in
    let podatki = ref false in
    let shema = ref [] in
    (for i=0 to n-1 do
        let line = List.nth data i in
        let prvi = List.hd line in
        let zacetek = String.lowercase (String.sub prvi 0 (min 10 (String.length prvi))) in
        if  zacetek = "@attribute" 
        then
            let temp = Str.split (Str.regexp " ") (List.hd line) in
            let l = List.length temp in
            let konec = List.nth temp (l-1) in
            (if List.mem (String.lowercase konec) ["numeric";"real";"integer"] 
            then
                parserji := (numpar mini maksi !attr) :: !parserji
            else
                parserji := strpar :: !parserji);
            attr := !attr+1
        else if (String.get zacetek 0) != '%' && (String.get zacetek 0) != '@'
        then
            begin
            (if (not !podatki)
            then
                (podatki := true;
                parserji := List.rev !parserji;
                attr := List.length !parserji;
                mini := (Array.make !attr 999999999999.);
                maksi := (Array.make !attr 0.))
            else ());
            let app f x = f x in
            let parsani = List.map2 app !parserji line in
            let (a,c) = loci parsani in
            clean := ((Tuple a),c) :: !clean
            end
        else ()
    done);
    (for i=0 to !attr-1 do
        let mi = (Array.get !mini i) in
        let ma = (Array.get !maksi i) in
        if mi=999999999999. && ma=0.
        then
            shema := !shema @ [D []]
        else
            shema := !shema @ [N (0.,ma-.mi)]
    done);
    let (s1,s2) = loci !shema in
    ((Tp s1),s2,!clean)
    
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
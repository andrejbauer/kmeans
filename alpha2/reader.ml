open Frame

(* iz seznama izloči n-ti element *)
let rec loci sez n = match sez with 
    | [] -> raise (Failure "Locujes prazen/prekratek seznam.")
    | x::xs when n = 0 -> (x,xs)
    | x::xs -> let (a,b) = loci xs (n-1) in (a,x::b)

let numpar mini maksi ind str = 
    try
        if str = "?" then Missing else
        let num = float_of_string str in
        (Array.set !mini ind (min num (Array.get !mini ind)));
        (Array.set !maksi ind (max num (Array.get !maksi ind)));
        Numerical num 
    with Failure rep -> print_endline str;Missing
    
let strpar str = if str = "?" then Missing else Discrete str 

let exp = Str.regexp "{[^{}]+}\|[^ \t\"']+\|'[^']+'\|\"[^\"]+\""

let attrparse line =
    let rec temp line n = 
        try
            (Str.search_forward exp line n);
            let a = (Str.matched_string line) in 
            let b = (temp line (Str.match_end () ) ) in 
            a :: b
        with 
            Not_found -> []
    in temp line 0


(* branje .arff datotek *)
let read_arff filename =  
    let chan = open_in filename in
    let parserji = ref [] in
    let cind = ref (-1) in
    let clean = ref [] in
    let mini = ref [||] in
    let maksi = ref [||] in 
    let attr = ref 0 in
    let podatki = ref false in
    let shema = ref [] in
    (try
    while true do
        let line = input_line chan in
        if line = "" then () else
        let zacetek = String.lowercase (String.sub line 0 (min 10 (String.length line))) in
        if  zacetek = "@attribute" then
            let temp = attrparse line in
            let name = List.nth temp 1 in
            let tip = List.nth temp 2 in
            (if List.mem (String.lowercase tip) ["numeric";"real";"integer"] 
            then
                parserji := (numpar mini maksi !attr) :: !parserji
            else
                parserji := strpar :: !parserji);
            attr := !attr+1;
            if String.lowercase name = "class" then cind := !attr -1 else ();
        else if (String.get zacetek 0) != '%' && (String.get zacetek 0) != '@'
        then
            begin
            (if (not !podatki)
            then
                (podatki := true;
                parserji := List.rev !parserji;
                attr := List.length !parserji;
                mini := (Array.make !attr 999999999999.);
                maksi := (Array.make !attr 0.);
                if !cind = (-1) then cind := !attr -1 else ())
            else ());
            let app f x = f x in
            let parsani = List.map2 app !parserji (Str.split (Str.regexp ",") line) in
            let (c,a) = loci parsani !cind in
            clean := ((Tuple a),c) :: !clean
            end
        else ()
    done;
    with 
        | End_of_file -> close_in chan);
    (for i=0 to !attr-1 do
        let mi = (Array.get !mini i) in
        let ma = (Array.get !maksi i) in
        if mi=999999999999. && ma=0.
        then
            shema := !shema @ [D []]
        else
            shema := !shema @ [N (0.,ma-.mi)]
    done);
    let (sc,sa) = loci !shema !cind in
    ((Tp sa),sc,!clean)

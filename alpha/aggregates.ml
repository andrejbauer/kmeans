open Frame

module Max : Aggregate =
struct
  let agg = List.fold_left max 0.
end

module Min : Aggregate =
struct
  let agg = List.fold_left min 1.
end

module Euc : Aggregate = 
struct
  let agg sez = 
    let square x = x ** 2. in
    sqrt (List.fold_left (+.) 0. (List.map square sez))  
end

module Sum : Aggregate =
struct
  let agg = List.fold_left (+.) 0.
end

module Avg : Aggregate = 
struct
  let agg sez = 
    let n = float (List.length sez) in
    (List.fold_left (+.) 0. sez) /. n
end

module Med : Aggregate =
struct
  let agg sez = 
    let n = List.length sez in
    let sorted = List.sort compare sez in
    if (n mod 2 = 0) 
      then 0.5 *. ( List.nth sorted (n/2-1) +. List.nth sorted (n/2))
      else List.nth sorted (n/2)
end
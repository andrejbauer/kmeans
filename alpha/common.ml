open Frame
open Pairings
open Aggregates

module AbsFloat : Metric with type point = float = 
struct
  type point = float
  let dist x y = abs_float (x -. y)
end

module CutFloat : Metric with type point = float =
struct
  type point = float
  let tol = 5.
  let dist x y = min 1. (abs_float (x -. y)/.tol)
end

let rampa tol = 
  (module struct
    type point = float
    let dist x y = min 1. (abs_float (x -. y)/.tol)
  end : Metric with type point = float)

module Jaccard (S : Set.S) : Metric with type point = S.t =
struct
  type point = S.t
  let dist s1 s2 = 
    let n = S.cardinal (S.union s1 s2) in
    let m = S.cardinal (S.inter s1 s2) in
    float (n-m) /. float n
end

module Haus 
  (S : Set.S) 
  (E : Metric with type point = S.elt)
  : Metric with type point = S.t = 
struct
  type point = S.t
  let dist s1 s2 = 
    let l1 = S.elements s1 in
    let l2 = S.elements s2 in
    let mini sez e = List.fold_left min 1. (List.map (E.dist e) sez) in
    let maksi sez1 sez2 = List.fold_left max 0. (List.map (mini sez2) sez1) in
    max (maksi l1 l2) (maksi l2 l1)    
end

module Str = Discrete 
  (struct
    type t = bytes
    let equal = (=)
  end)
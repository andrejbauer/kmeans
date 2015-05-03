module type Pairing =
sig
  val pair : 'a list -> 'a list -> ('a * 'a) list
end


module type Aggregate =
sig
  val agg : float list -> float
end


module type Metric =
sig
  type point
  val dist : point -> point -> float
end

module Discrete 
  (T : sig 
    type t
    val equal : t -> t -> bool
  end) 
  : Metric with type point = T.t =
struct
  type point = T.t
  let dist x y = if T.equal x y then 1.0 else 0.0
end

module Product2
  (S1 : Metric) 
  (S2 : Metric) 
  (Agg : Aggregate)
  : Metric with type point = S1.point * S2.point =
struct
  type point = S1.point * S2.point
  let dist (a1,a2) (b1,b2) = Agg.agg [S1.dist a1 b1; S2.dist a2 b2]
end

module Product3
  (S1 : Metric) 
  (S2 : Metric)
  (S3 : Metric) 
  (Agg : Aggregate)
  : Metric with type point = S1.point * S2.point * S3.point =
struct
  type point = S1.point * S2.point * S3.point
  let dist (a1,a2,a3) (b1,b2,b3) = Agg.agg [S1.dist a1 b1; S2.dist a2 b2; S3.dist a3 b3]
end
(* itd *)


module SetMetric 
  (S : Set.S)
  (E : Metric with type point = S.elt)
  (Par : Pairing)
  (Agg : Aggregate)
  : Metric  with type point = S.t =
struct

  type point = S.t
  
  let dist s1 s2 =
    let p = Par.pair (S.elements s1) (S.elements s2) in
    let dist' (a,b) = E.dist a b in
    Agg.agg (List.map dist' p) 
end



module ListMetric
  (E : Metric)
  (Par : Pairing)
  (Agg : Aggregate)
  : Metric with type point = E.point list =
struct
  type point = E.point list
  
  let dist l1 l2 = 
    let p = Par.pair l1 l2 in
    let dist' (a,b) = E.dist a b in
    Agg.agg (List.map dist' p)
end


module type Kmeans_Frame = 
sig
  type attr 
  type clas
  val predict : attr * clas list -> int -> ((clas -> clas -> float) -> (attr -> attr -> float) -> attr * clas list -> clas) -> attr -> clas
end

module Kmeans 
  (A : Metric)
  (C : Metric)
  : (Kmeans_Frame with type attr = A.point and type clas = C.point) =
struct
  type attr = A.point
  type clas = C.point
  
  let predict tset k mean sample =
    let key x y = Pervasives.compare (A.dist sample (fst x)) (A.dist sample (fst y)) in
    let rec take n lst = if n = 0 then [] else (List.hd lst) :: (take (n-1) (List.tl lst)) in
    let neighs = List.sort key tset in
    mean C.dist A.dist (take k neighs)
end



(* NEKAJ KONKRETNIH STVARI *)

module All2All : Pairing = 
struct
  let pair s1 s2 =
    let f x y = (x,y) in
    let pomo sez x = List.map (f x) sez in
    List.flatten (List.map (pomo s1) s2) 
end

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

module T = Set.Make(struct
                      type t = float
                      let compare = Pervasives.compare
                    end)

let s1 =T.add 1. (T.add 2. T.empty);;
let s2 = T.add 3. (T.add 1. (T.singleton (-1.)));;
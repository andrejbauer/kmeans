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
  let dist x y = if T.equal x y then 0.0 else 1.0
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

(* zelo neroden produkt za spodnji primer na podatkih *)
module Product15
  (S1 : Metric) 
  (S2 : Metric)
  (S3 : Metric)
  (S4 : Metric) 
  (S5 : Metric)
  (S6 : Metric) 
  (S7 : Metric) 
  (S8 : Metric)
  (S9 : Metric) 
  (S10 : Metric) 
  (S11 : Metric)
  (S12 : Metric) 
  (S13 : Metric) 
  (S14 : Metric)
  (S15 : Metric)
  (Agg : Aggregate)
  : Metric with type point = S1.point * S2.point * S3.point * S4.point * S5.point * S6.point * S7.point * S8.point * S9.point*S10.point * S11.point * S12.point*S13.point * S14.point * S15.point =
struct
  type point = S1.point * S2.point * S3.point * S4.point * S5.point * S6.point * S7.point * S8.point * S9.point*S10.point * S11.point * S12.point*S13.point * S14.point * S15.point
  
  let dist (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15) = 
  Agg.agg [S1.dist a1 b1; S2.dist a2 b2; S3.dist a3 b3; S4.dist a4 b4; S5.dist a5 b5; S6.dist a6 b6; S7.dist a7 b7; S8.dist a8 b8; S9.dist a9 b9; S10.dist a10 b10; S11.dist a11 b11; S12.dist a12 b12; S13.dist a13 b13; S14.dist a14 b14; S15.dist a15 b15]
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
  val predict : (attr * clas) list -> int -> ((clas -> clas -> float) -> (attr -> attr -> float) -> (attr * clas) list -> clas) -> attr -> clas
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
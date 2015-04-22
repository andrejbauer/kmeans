(** Constructions of distance functions. *)

(* A metric space is given by a type and a "distance" function. 
   We do not insist that the distance function actually be a metric.
   For instance, it could be a square distance.
*)
module type METRIC =
sig
  type t
  val dist : t -> t -> float
end

module Float : METRIC =
struct
  type t = float
  let dist x y = abs_float (x -. y)
end

module Discrete (T : sig 
                       type t
                       val equal : t -> t -> bool
                     end) : METRIC with type t = T.t =
struct
  type t = T.t
  let dist x y = if T.equal x y then 1.0 else 0.0
end

let product ?(aggregate=(+.)) (module M1 : METRIC) (module M2 : METRIC)
  =
  (module struct
     type t = M1.t * M2.t
     let dist (x1,y1) (x2,y2) = aggregate (M1.dist x1 x2) (M2.dist y1 y2)
   end : METRIC)

let space_x = product (module Float) (module Float) ;;

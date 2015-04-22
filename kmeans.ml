module type COLLECTION =
sig
  type element
  type t
  val empty : t
  val add : element -> t -> t
  val singleton : element -> t
  val select : int -> t -> element list * t
  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a
end

module SetCollection(Element : sig type t end) :
  COLLECTION with type element = Element.t =
struct
  module S = Set.Make(struct
                       type t = Element.t
                       let compare = Pervasives.compare
                     end)
                     
  type t = S.t
  type element = Element.t
  let empty = S.empty
  let add = S.add
  let singleton = S.singleton
  let fold = S.fold

  let select k s0 =
    let s = ref s0
    and xs = ref []
    in
    for i = 1 to k do
      let x = S.choose !s in
      s := S.remove x !s ;
      xs := x :: !xs
    done ;
    !xs, !s
end

module type MEAN_SPACE =
sig
  type point
  module PointSet : COLLECTION with type element = point

  val dist : point -> point -> float (** Distance *)
  val mean : PointSet.t -> point (** Mean value of a collection *)
end

(** Floating points in their usual metric *)
module FloatSpace : MEAN_SPACE with type point = float =
struct
  type point = float

  module PointSet = SetCollection(struct
                                   type t = float
                                  end)

  let dist x y = abs_float (x -. y)

  let mean s =
    let (n, sum) = PointSet.fold (fun x (n, sum) -> (n + 1, sum +. x)) s (0, 0.0)
    in sum /. float n

end

(** A functor which cuts off a metric at a maximal distance *)
module MaxDistance
         (M : sig val max_dist : float end)
         (S : MEAN_SPACE) : MEAN_SPACE with type point = S.point =
struct
  type point = S.point
  module PointSet = S.PointSet

  let dist x y = min M.max_dist (S.dist x y)

  let mean = S.mean
end

(* The product of two spaces *)
module Product
         (S1 : MEAN_SPACE)
         (S2 : MEAN_SPACE) : MEAN_SPACE with type point = S1.point * S2.point =
struct
  type point = S1.point * S2.point

  module PointSet = SetCollection(struct type t = point end)

  let dist (x1,y1) (x2,y2) = S1.dist x1 x2 +. S2.dist y1 y2

  let mean s =
    let s1 = PointSet.fold (fun (x, _) -> S1.PointSet.add x) s S1.PointSet.empty
    and s2 = PointSet.fold (fun (_, y) -> S2.PointSet.add y) s S2.PointSet.empty
    in (S1.mean s1, S2.mean s2)
end

module type KMEANS =
sig
  type point
  module PointSet : COLLECTION with type element = point
  val kmeans : int -> PointSet.t -> PointSet.t list * float
end

module Lloyd(Space : MEAN_SPACE) : KMEANS with type point = Space.point 
                                       and module PointSet = Space.PointSet =
struct
  type point = Space.point
  module PointSet = Space.PointSet

  let sum = List.fold_left (+.) 0.0

  (* The distance of a point to a cluster. *)
  let cluster_distance x cluster =
    PointSet.fold (fun y d -> d +. Space.dist x y) cluster 0.0

  let kmeans k original_points =

    (* Pick k points as initial centroids *)
    let centroids, points = PointSet.select k original_points in

    (* The clusters *)
    let clusters = Array.of_list (List.map PointSet.singleton centroids) in

    (* Find the index of the cluster that x belongs to *)
    let best_cluster x =
      let i = ref 0
      and d = ref (cluster_distance x clusters.(0)) in
       for j = 1 to k - 1 do
         let d' = cluster_distance x clusters.(j) in
         if d' < !d then (d := d' ; i := j)
       done ;
       (!i, !d)
    in

    PointSet.fold
      (fun x _ ->
       let (i, _) = best_cluster x in
       clusters.(i) <- PointSet.add x clusters.(i))
      points
      () ;

    (** XXX not finished yet *)
    Array.to_list clusters, 0.0

end
